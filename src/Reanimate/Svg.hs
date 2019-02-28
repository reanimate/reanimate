module Reanimate.Svg where

import           Codec.Picture             (PixelRGBA8 (..))
import           Control.Lens              (over, set, (%~), (&), (.~), (^.))
import           Control.Monad.Fix
import           Control.Monad.State
import           Control.Arrow
import           Data.Attoparsec.Text      (parseOnly)
import           Data.List
import qualified Data.Map                  as Map
import           Data.Maybe
import qualified Data.Text                 as T
import           Graphics.Svg
import Codec.Picture.Types
import           Graphics.Svg.PathParser
import           Linear.Metric
import           Linear.V2
import           Linear.Vector
import           Reanimate.Svg.NamedColors
import qualified Reanimate.Transform       as Transform

import           Debug.Trace

defaultDPI :: Dpi
defaultDPI = 96

replaceUses :: Document -> Document
replaceUses doc = doc & elements %~ map (mapTree replace)
                      & definitions .~ Map.empty
  where
    replace (UseTree _ Just{}) = error "replaceUses: subtree in use?"
    replace (UseTree use Nothing) =
      case Map.lookup (use^.useName) idMap of
        Nothing -> error $ "Unknown id: " ++ (use^.useName)
        Just tree ->
          GroupTree $
          defaultSvg & groupChildren .~ [tree]
                     & drawAttr .~ (defaultSvg & transform .~ Just [baseToTransformation (use^.useBase)])
    replace x = x
    baseToTransformation (x,y) =
      case (toUserUnit defaultDPI x, toUserUnit defaultDPI y) of
        (Num a, Num b) -> Translate a b
        _              -> TransformUnknown
    docTree = GroupTree $ set groupChildren (doc^.elements) defaultSvg
    idMap = foldTree updMap Map.empty docTree `Map.union`
            Map.mapMaybe elementToTree (doc^.definitions)
    updMap m tree =
      case tree^.drawAttr.attrId of
        Nothing  -> m
        Just tid -> Map.insert tid tree m
    elementToTree (ElementGeometry t) = Just t
    elementToTree _                   = Nothing

docIds :: Document -> [String]
docIds doc = Map.keys idMap ++ Map.keys (doc^.definitions)
  where
    docTree = GroupTree $ set groupChildren (doc^.elements) defaultSvg
    idMap = foldTree updMap Map.empty docTree
    updMap m tree =
      case tree^.drawAttr.attrId of
        Nothing  -> m
        Just tid -> Map.insert tid tree m


-- Transform out viewbox. defs and CSS rules are discarded.
unbox :: Document -> Tree
unbox doc@Document{_viewBox = Just (minx, minw, _width, _height)} =
  GroupTree $ defaultSvg
          & groupChildren .~ doc^.elements
          & drawAttr .~ (defaultSvg & transform .~ Just [Translate (-minx) (-minw)])
unbox doc =
  GroupTree $ defaultSvg
    & groupChildren .~ doc^.elements

type CmdM a = State RPoint a

data LineCommand
  = LineMove RPoint
  | LineDraw RPoint
  | LineBezier [RPoint]
  deriving (Show)

lineToPath :: [LineCommand] -> [PathCommand]
lineToPath = map worker
  where
    worker (LineMove p)         = MoveTo OriginAbsolute [p]
    worker (LineDraw p)         = LineTo OriginAbsolute [p]
    worker (LineBezier [a,b,c]) = CurveTo OriginAbsolute [(a,b,c)]
    worker (LineBezier [a,b])   = QuadraticBezier OriginAbsolute [(a,b)]

partialLine :: Double -> [LineCommand] -> [LineCommand]
partialLine alpha cmds = evalState (worker 0 cmds) zero
  where
    worker d [] = pure []
    worker d (cmd:xs) = do
      from <- get
      len <- lineLength cmd
      let frac = (targetLen-d) / len
      if len == 0 || frac > 1
        then (cmd:) <$> worker (d+len) xs
        else pure [adjustLineLength frac from cmd]
    totalLen = evalState (sum <$> mapM lineLength cmds) zero
    targetLen = totalLen * alpha

adjustLineLength :: Double -> RPoint -> LineCommand -> LineCommand
adjustLineLength alpha from cmd =
  case cmd of
    LineBezier points -> LineBezier $ drop 1 $ partial_bezier_points (from:points) 0 alpha
    LineMove p -> LineMove p
    LineDraw t -> LineDraw (lerp alpha t from)

lineLength :: LineCommand -> CmdM Double
lineLength cmd =
  case cmd of
    LineMove to       -> pure 0 <* put to
    LineDraw to       -> gets (distance to) <* put to
    LineBezier points -> gets (distance (last points)) <* put (last points)

toLineCommands :: [PathCommand] -> [LineCommand]
toLineCommands ps = evalState (worker zero Nothing ps) zero
  where
    worker startPos mbPrevControlPt [] = pure []
    worker startPos mbPrevControlPt (cmd:cmds) = do
      lcmds <- toLineCommand startPos mbPrevControlPt cmd
      let startPos' =
            case lcmds of
              [LineMove pos] -> pos
              _              -> startPos
      (lcmds++) <$> worker startPos' (cmdToControlPoint $ last lcmds) cmds

cmdToControlPoint (LineBezier points) = Just (last (init points))
cmdToControlPoint _                   = Nothing

toLineCommand :: RPoint -> Maybe RPoint -> PathCommand -> CmdM [LineCommand]
toLineCommand startPos mbPrevControlPt cmd = do
  case cmd of
    MoveTo OriginAbsolute []  -> pure []
    MoveTo OriginAbsolute lst -> put (last lst) *> gets (pure.LineMove)
    MoveTo OriginRelative lst -> modify (+ sum lst) *> gets (pure.LineMove)
    LineTo OriginAbsolute lst -> forM lst (\to -> put to *> pure (LineDraw to))
    LineTo OriginRelative lst -> forM lst (\to -> modify (+to) *> gets LineDraw)
    HorizontalTo OriginAbsolute lst ->
      forM lst $ \x -> modify (_x .~ x) *> gets LineDraw
    HorizontalTo OriginRelative lst ->
      forM lst $ \x -> modify (_x %~ (+x)) *> gets LineDraw
    VerticalTo OriginAbsolute lst ->
      forM lst $ \y -> modify (_y .~ y) *> gets LineDraw
    VerticalTo OriginRelative lst ->
      forM lst $ \y -> modify (_y %~ (+y)) *> gets LineDraw
    CurveTo OriginAbsolute quads -> do
      forM quads $ \(a,b,c) -> put c *> pure (LineBezier [a,b,c])
    CurveTo OriginRelative quads -> do
      forM quads $ \(a,b,c) -> do
        from <- get <* modify (+c)
        pure $ LineBezier $ map (+from) [a,b,c]
    SmoothCurveTo o lst -> mfix $ \result -> do
      let ctrl = mbPrevControlPt : map cmdToControlPoint result
      forM (zip lst ctrl) $ \((c2,to), mbControl) -> do
        from <- get <* adjustPosition o to
        let c1 = maybe (makeAbsolute o from c2) (mirrorPoint from) mbControl
        pure $ LineBezier [c1,makeAbsolute o from c2,makeAbsolute o from to]
    QuadraticBezier OriginAbsolute pairs -> do
      forM pairs $ \(a,b) -> put b *> pure (LineBezier [a,b])
    QuadraticBezier OriginRelative pairs -> do
      forM pairs $ \(a,b) -> do
        from <- get <* modify (+b)
        pure $ LineBezier $ map (+from) [a,b]
    SmoothQuadraticBezierCurveTo o lst -> mfix $ \result -> do
      let ctrl = mbPrevControlPt : map cmdToControlPoint result
      forM (zip lst ctrl) $ \(to, mbControl) -> do
        from <- get <* adjustPosition o to
        let c1 = maybe from (mirrorPoint from) mbControl
        pure $ LineBezier [c1,makeAbsolute o from to]
    EllipticalArc o points -> concat <$>
      (forM points $ \(rotX, rotY, angle, largeArc, sweepFlag, to) -> do
        from <- get <* adjustPosition o to
        return $ convertSvgArc from rotX rotY angle largeArc sweepFlag (makeAbsolute o from to))
    EndPath -> put startPos *> pure [LineDraw startPos]
  where
    mirrorPoint c p = c*2-p
    adjustPosition OriginRelative p = modify (+p)
    adjustPosition OriginAbsolute p = put p
    makeAbsolute OriginAbsolute from p = p
    makeAbsolute OriginRelative from p = from+p


calculateVectorAngle :: Double -> Double -> Double -> Double -> Double
calculateVectorAngle ux uy vx vy
    | tb >= ta
        = tb - ta
    | otherwise
        = pi * 2 - (ta - tb)
    where
        ta = atan2 uy ux
        tb = atan2 vy vx

-- ported from: https://github.com/vvvv/SVG/blob/master/Source/Paths/SvgArcSegment.cs
convertSvgArc :: RPoint -> Coord -> Coord -> Coord -> Bool -> Bool -> RPoint -> [LineCommand]
convertSvgArc (V2 x0 y0) radiusX radiusY angle largeArcFlag sweepFlag (V2 x y)
    | x0 == x && y0 == y
        = []
    | radiusX == 0.0 && radiusY == 0.0
        = [LineDraw (V2 x y)]
    | otherwise
        = calcSegments x0 y0 theta1' segments'
    where
        sinPhi = sin (angle * pi/180)
        cosPhi = cos (angle * pi/180)

        x1dash = cosPhi * (x0 - x) / 2.0 + sinPhi * (y0 - y) / 2.0
        y1dash = -sinPhi * (x0 - x) / 2.0 + cosPhi * (y0 - y) / 2.0

        numerator = radiusX * radiusX * radiusY * radiusY - radiusX * radiusX * y1dash * y1dash - radiusY * radiusY * x1dash * x1dash

        s = sqrt(1.0 - numerator / (radiusX * radiusX * radiusY * radiusY))
        rx   = if (numerator < 0.0) then (radiusX * s) else radiusX
        ry   = if (numerator < 0.0) then (radiusY * s) else radiusY
        root = if (numerator < 0.0)
                then (0.0)
                else ((if ((largeArcFlag && sweepFlag) || (not largeArcFlag && not sweepFlag)) then (-1.0) else 1.0) *
                        sqrt(numerator / (radiusX * radiusX * y1dash * y1dash + radiusY * radiusY * x1dash * x1dash)))

        cxdash = root * rx * y1dash / ry
        cydash = -root * ry * x1dash / rx

        cx = cosPhi * cxdash - sinPhi * cydash + (x0 + x) / 2.0
        cy = sinPhi * cxdash + cosPhi * cydash + (y0 + y) / 2.0

        theta1'  = calculateVectorAngle 1.0 0.0 ((x1dash - cxdash) / rx) ((y1dash - cydash) / ry)
        dtheta' = calculateVectorAngle ((x1dash - cxdash) / rx) ((y1dash - cydash) / ry) ((-x1dash - cxdash) / rx) ((-y1dash - cydash) / ry)
        dtheta  = if (not sweepFlag && dtheta' > 0)
                    then  (dtheta' - 2 * pi)
                    else  (if (sweepFlag && dtheta' < 0) then (dtheta' + 2 * pi) else dtheta')

        segments' = ceiling (abs (dtheta / (pi / 2.0)))
        delta = dtheta / fromInteger segments'
        t = 8.0 / 3.0 * sin(delta / 4.0) * sin(delta / 4.0) / sin(delta / 2.0)

        calcSegments startX startY theta1 segments
            | segments == 0
                = []
            | otherwise
                = LineBezier [ V2 (startX + dx1) (startY + dy1)
                             , V2 (endpointX + dxe) (endpointY + dye)
                             , V2 endpointX endpointY ] : calcSegments endpointX endpointY theta2 (segments - 1)
            where
                cosTheta1 = cos theta1
                sinTheta1 = sin theta1
                theta2 = theta1 + delta
                cosTheta2 = cos theta2
                sinTheta2 = sin theta2

                endpointX = cosPhi * rx * cosTheta2 - sinPhi * ry * sinTheta2 + cx
                endpointY = sinPhi * rx * cosTheta2 + cosPhi * ry * sinTheta2 + cy

                dx1 = t * (-cosPhi * rx * sinTheta1 - sinPhi * ry * cosTheta1)
                dy1 = t * (-sinPhi * rx * sinTheta1 + cosPhi * ry * cosTheta1)

                dxe = t * (cosPhi * rx * sinTheta2 + sinPhi * ry * cosTheta2)
                dye = t * (sinPhi * rx * sinTheta2 - cosPhi * ry * cosTheta2)


-- Algorithm taken from manim. It's magic.
bezier :: [RPoint] -> Double -> RPoint
bezier points t = sum
    [ point ^* (((1-t)**(fromIntegral $ n-k)) * (t**fromIntegral k) * fromIntegral (choose n k))
    | (k, point) <- zip [0..] points ]
  where
    n = length points -1
    choose n k = product [n,n-1 .. n-k+1] `div` product [1..k]

partial_bezier_points :: [RPoint] -> Double -> Double -> [RPoint]
partial_bezier_points points a b
  | isNaN end_prop || isInfinite end_prop = replicate (length points) (last points)
  | otherwise = [ bezier (take (i+1) a_to_1) end_prop | i <- [0..length points-1] ]
  where
    a_to_1 = [ bezier (drop i points) a | i <- [0..length points-1] ]
    end_prop = (b-a) / (1-a)



interpolatePathCommands :: Double -> [PathCommand] -> [PathCommand]
interpolatePathCommands alpha = lineToPath . partialLine alpha . toLineCommands

partialSvg :: Double -> Tree -> Tree
partialSvg alpha = mapTree worker
  where
    worker (PathTree path) =
      PathTree $ path & pathDefinition %~ lineToPath . partialLine alpha . toLineCommands
    worker t = t

-- (x,y,w,h)
boundingBox :: Tree -> (Double, Double, Double, Double)
boundingBox t =
    case svgBoundingPoints t of
      [] -> (0,0,0,0)
      (V2 x y:rest) ->
        let (minx, miny, maxx, maxy) = foldl' worker (x, y, x, y) rest
        in (minx, miny, maxx-minx, maxy-miny)
  where
    worker (minx, miny, maxx, maxy) (V2 x y) =
      (min minx x, min miny y, max maxx x, max maxy y)

linePoints :: [LineCommand] -> [RPoint]
linePoints = worker zero
  where
    worker from [] = []
    worker from (x:xs) =
      case x of
        LineMove to     -> worker to xs
        LineDraw to     -> from:to:worker to xs
        LineBezier ctrl -> -- approximation
          [ last (partial_bezier_points (from:ctrl) 0 (recip chunks*i)) | i <- [0..chunks]] ++
          worker (last ctrl) xs
    chunks = 10

svgBoundingPoints :: Tree -> [RPoint]
svgBoundingPoints t = map (Transform.transformPoint m) $
    case t of
      None            -> []
      UseTree{}       -> []
      GroupTree g     -> concatMap svgBoundingPoints (g^.groupChildren)
      SymbolTree (Symbol g) -> concatMap svgBoundingPoints (g^.groupChildren)
      PathTree p      -> linePoints $ toLineCommands (p^.pathDefinition)
      CircleTree{}    -> error "CircleTree"
      PolyLineTree{}  -> error "PolyLineTree"
      EllipseTree{}   -> error "EllipseTree"
      LineTree{}      -> error "LineTree"
      RectangleTree rect ->
        case mapTuple (toUserUnit defaultDPI) (rect^.rectUpperLeftCorner) of
          (Num x, Num y) -> [V2 x y] ++
            case mapTuple (toUserUnit defaultDPI) (rect^.rectWidth, rect^.rectHeight) of
              (Num w, Num h) -> [V2 (x+w) (y+h)]
              _ -> []
          _ -> []
      TextTree{}      -> []
      ImageTree{}     -> []
      MeshGradientTree{} -> []
  where
    m = Transform.mkMatrix (t^.drawAttr.transform)
    mapTuple f = f *** f

withTransformations :: [Transformation] -> Tree -> Tree
withTransformations transformations t =
  mkGroup [t] & drawAttr %~ transform .~ Just transformations

translate :: Double -> Double -> Tree -> Tree
translate x y = withTransformations [Translate x y]

rotate :: Double -> Tree -> Tree
rotate a = withTransformations [Rotate a Nothing]

rotateAround :: Double -> RPoint -> Tree -> Tree
rotateAround a (V2 x y) = withTransformations [Rotate a (Just (x,y))]

rotateAroundCenter :: Double -> Tree -> Tree
rotateAroundCenter a t =
    rotateAround a (V2 (x+w/h) (y+h/2)) t
  where
    (x,y,w,h) = boundingBox t

scale :: Double -> Tree -> Tree
scale a = withTransformations [Scale a Nothing]

scaleXY :: Double -> Double -> Tree -> Tree
scaleXY x y = withTransformations [Scale x (Just y)]

-- scalePoints :: Double -> Tree -> Tree
-- scalePoints a = scalePointsXY a a
--
-- scalePointsXY :: Double -> Double -> Tree -> Tree
-- scalePointsXY x y = mapTree worker
--   where
--     worker t =
--       case t of
--         None            -> t
--         UseTree{}       -> t
--         GroupTree{}     -> t
--         SymbolTree{}    -> t
--         PathTree p      -> PathTree $ p
--           & pathDefinition %~ lineToPath . map scaleCmd . toLineCommands
--         CircleTree{}    -> error "scalePointsXY CircleTree"
--         PolyLineTree{}  -> error "scalePointsXY PolyLineTree"
--         EllipseTree{}   -> error "scalePointsXY EllipseTree"
--         LineTree{}      -> error "scalePointsXY LineTree"
--         RectangleTree rect -> RectangleTree $ rect
--           & rectUpperLeftCorner %~ (mapNumber (*x) *** mapNumber (*y))
--           & rectWidth %~ mapNumber (*x)
--           & rectHeight %~ mapNumber (*y)
--         TextTree{}      -> t
--         ImageTree{}     -> t
--         MeshGradientTree{} -> t
--     scaleCmd (LineMove to) = LineMove (to * V2 x y)
--     scaleCmd (LineDraw to) = LineDraw (to * V2 x y)
--     scaleCmd (LineBezier points) = LineBezier (map (*V2 x y) points)

center :: Tree -> Tree
center t = translate (-x-w/2) (-y-h/2) t
  where
    (x, y, w, h) = boundingBox t

mkColor :: String -> Texture
mkColor name =
  case Map.lookup name svgNamedColors of
    Nothing -> ColorRef (PixelRGBA8 240 248 255 255)
    Just c  -> ColorRef c

withStrokeColor :: String -> Tree -> Tree
withStrokeColor color = drawAttr %~ strokeColor .~ pure (mkColor color)

withFillColor :: String -> Tree -> Tree
withFillColor color = drawAttr %~ fillColor .~ pure (mkColor color)

withFillColorPixel :: PixelRGBA8 -> Tree -> Tree
withFillColorPixel color = drawAttr %~ fillColor .~ pure (ColorRef color)

withFillOpacity :: Double -> Tree -> Tree
withFillOpacity opacity = drawAttr %~ fillOpacity .~ Just (realToFrac opacity)

withStrokeWidth :: Number -> Tree -> Tree
withStrokeWidth width = drawAttr %~ strokeWidth .~ pure width

withClipPathRef :: ElementRef -> Tree -> Tree
withClipPathRef ref = drawAttr %~ clipPathRef .~ pure ref

mkRect :: Point -> Number -> Number -> Tree
mkRect corner width height = RectangleTree $ defaultSvg
  & rectUpperLeftCorner .~ corner
  & rectWidth .~ width
  & rectHeight .~ height

mkBoundingRect :: Tree -> Double -> Tree
mkBoundingRect src margin =
    mkRect (Num $ x-margin, Num $ y-margin) (Num $ w+margin*2) (Num $ h+margin*2)
  where
    (x, y, w, h) = boundingBox src

mkLine :: Point -> Point -> Tree
mkLine point1 point2 = LineTree $ defaultSvg
  & linePoint1 .~ point1
  & linePoint2 .~ point2

mkGroup :: [Tree] -> Tree
mkGroup forest = GroupTree $ defaultSvg
  & groupChildren .~ forest

mkPathString :: String -> Tree
mkPathString = mkPathText . T.pack

mkPathText :: T.Text -> Tree
mkPathText str =
  case parseOnly pathParser str of
    Left err   -> error err
    Right cmds -> PathTree $ defaultSvg & pathDefinition .~ cmds

mkLinePath :: [(Double, Double)] -> Tree
mkLinePath [] = mkGroup []
mkLinePath ((startX, startY):rest) =
    PathTree $ defaultSvg & pathDefinition .~ cmds
  where
    cmds = [ MoveTo OriginAbsolute [V2 startX startY]
           , LineTo OriginAbsolute [ V2 x y | (x, y) <- rest ] ]

mkBackground :: String -> Tree
mkBackground color = withFillColor color $ mkRect (Num $ -320/2, Num $ -180/2) (Percent 1) (Percent 1)

mkBackgroundPixel :: PixelRGBA8 -> Tree
mkBackgroundPixel pixel =
    withFillColorPixel pixel $ mkRect (Num $ -320/2, Num $ -180/2) (Percent 1) (Percent 1)

withSubglyphs :: [Int] -> (Tree -> Tree) -> Tree -> Tree
withSubglyphs target fn t = evalState (worker t) 0
  where
    worker :: Tree -> State Int Tree
    worker t =
      case t of
        GroupTree g -> do
          cs <- mapM worker (g ^. groupChildren)
          return $ GroupTree $ g & groupChildren .~ cs
        PathTree{} -> handleGlyph t
        CircleTree{} -> handleGlyph t
        PolyLineTree{} -> handleGlyph t
        PolygonTree{} -> handleGlyph t
        EllipseTree{} -> handleGlyph t
        LineTree{} -> handleGlyph t
        RectangleTree{} -> handleGlyph t
        _ -> return t
    handleGlyph :: Tree -> State Int Tree
    handleGlyph t = do
      n <- get <* modify (+1)
      if n `elem` target
        then return $ fn t
        else return t
