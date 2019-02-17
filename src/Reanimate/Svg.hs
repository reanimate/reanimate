module Reanimate.Svg where

import           Control.Lens               (over, set, (%~), (&), (.~), (^.))
import           Control.Monad.State
import           Control.Monad.Fix
import qualified Data.Map                   as Map
import           Data.Maybe
import           Data.List
import           Graphics.Svg
import           Linear.Metric
import           Linear.V2
import           Linear.Vector

import           Debug.Trace

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
    defaultDPI = 96
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
    worker (LineMove p) = MoveTo OriginAbsolute [p]
    worker (LineDraw p) = LineTo OriginAbsolute [p]
    worker (LineBezier [a,b,c]) = CurveTo OriginAbsolute [(a,b,c)]
    worker (LineBezier [a,b]) = QuadraticBezier OriginAbsolute [(a,b)]

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
cmdToControlPoint _ = Nothing

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
    EllipticalArc origin points -> undefined
    EndPath -> put startPos *> pure [LineDraw startPos]
  where
    mirrorPoint c p = c*2-p
    adjustPosition OriginRelative p = modify (+p)
    adjustPosition OriginAbsolute p = put p
    makeAbsolute OriginAbsolute from p = p
    makeAbsolute OriginRelative from p = from+p


-- Algorithm taken from manim. It's magic.
bezier :: [RPoint] -> Double -> RPoint
bezier points t = sum
    [ point ^* (((1-t)**(fromIntegral $ n-k)) * (t**fromIntegral k) * fromIntegral (choose n k))
    | (k, point) <- zip [0..] points ]
  where
    n = length points -1
    choose n k = product [n,n-1 .. n-k+1] `div` product [1..k]
partial_bezier_points points a b
  | isNaN end_prop || isInfinite end_prop = replicate (length points) (last points)
  | otherwise = [ bezier (take (i+1) a_to_1) end_prop | i <- [0..length points-1] ]
  where
    a_to_1 = [ bezier (drop i points) a | i <- [0..length points-1] ]
    end_prop = (b-a) / (1-a)



interpolatePathCommands :: Double -> [PathCommand] -> [PathCommand]
interpolatePathCommands alpha = lineToPath . partialLine alpha . toLineCommands
--     evalState (worker 0 cmds) zero
--   where
--     worker d [] = pure []
--     worker d (x:xs) | d > minDistance = pure []
--     worker d (LineTo OriginAbsolute []:xs) = worker d xs
--     worker d (LineTo OriginAbsolute (to:tos):xs) = do
--       from <- get <* put to
--       let d' = distance from to
--           out = LineTo OriginAbsolute [lerp (min 1 ((minDistance-d)/d')) to from]
--           rest = LineTo OriginAbsolute tos
--       (out:) <$> worker (d+d') (rest:xs)
--     worker d (LineTo OriginRelative []:xs) = worker d xs
--     worker d (LineTo OriginRelative (to:tos):xs) = do
--       from <- get <* modify (+to)
--       let d' = distance zero to
--           out = LineTo OriginRelative [lerp (min 1 ((minDistance-d)/d')) to zero]
--           rest = LineTo OriginRelative tos
--       (out:) <$> worker (d+d') (rest:xs)
--     worker d (x:xs) = do
--       d' <- estimateCmdLength x
--       (x:) <$> worker (d+d') xs
--     minDistance = estimatePathLength cmds * alpha
--
-- splitPathCommands :: [PathCommand] -> [PathCommand]
-- splitPathCommands cmds = evalState (concat <$> mapM splitPathCommand cmds) zero
--
-- splitPathCommand :: PathCommand -> CmdM [PathCommand]
-- splitPathCommand cmd = do
--   case cmd of
--     MoveTo OriginAbsolute [] -> pure [cmd]
--     MoveTo OriginAbsolute lst -> put (last lst) *> pure [cmd]
--     MoveTo OriginRelative lst -> modify (+ sum lst) *> pure [cmd]
--     -- LineTo OriginAbsolute lst -> do
--     --   concat <$> forM lst (\to -> do
--     --     from <- get <* put (to :: RPoint)
--     --     let towardsTo = (to - from) / fromIntegral pieces :: RPoint
--     --     return $ replicate pieces (LineTo OriginRelative [towardsTo]))
--     -- LineTo OriginRelative lst -> do
--     --   concat <$> forM lst (\to -> do
--     --     from <- get <* modify (+to)
--     --     let towardsTo = to / fromIntegral pieces :: RPoint
--     --     return $ replicate pieces (LineTo OriginRelative [towardsTo]))
--     _ -> pure [cmd]
--   where
--     pieces = 100 :: Int
--
-- estimatePathLength :: [PathCommand] -> Double
-- estimatePathLength cmds = evalState (sum <$> mapM estimateCmdLength cmds) zero
--
-- estimateCmdLength :: PathCommand -> CmdM Double
-- estimateCmdLength cmd =
--   case cmd of
--     MoveTo OriginAbsolute []  -> pure 0
--     MoveTo OriginAbsolute lst -> put (last lst) *> pure 0
--     MoveTo OriginRelative lst -> modify (+ sum lst) *> pure 0
--     LineTo OriginAbsolute lst ->
--       sum <$> forM lst (\to -> gets (distance to) <* put to)
--     LineTo OriginRelative lst ->
--       sum <$> forM lst (\to -> modify (+to) *> pure (distance zero to))
--     HorizontalTo OriginAbsolute lst -> undefined
--     HorizontalTo OriginRelative lst -> undefined
--     VerticalTo OriginAbsolute lst -> undefined
--     VerticalTo OriginRelative lst -> undefined
--     CurveTo origin quads -> undefined
--     SmoothCurveTo origin lst -> undefined
--     QuadraticBezier origin pairs -> undefined
--     SmoothQuadraticBezierCurveTo origin points -> undefined
--     EllipticalArc origin points -> undefined
--     EndPath -> pure 0

-- lineToPath :: [LineCommand] -> [PathCommand]
-- partialLine :: Double -> [LineCommand] -> [LineCommand]

partialSvg :: Double -> Tree -> Tree
partialSvg alpha = mapTree worker
  where
    worker (PathTree path) =
      PathTree $ path & pathDefinition %~ lineToPath . partialLine alpha . toLineCommands
    worker t = t
