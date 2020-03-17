{-# LANGUAGE LambdaCase #-}
module Reanimate.Svg
  ( module Reanimate.Svg
  , module Reanimate.Svg.Constructors
  , module Reanimate.Svg.LineCommand
  , module Reanimate.Svg.BoundingBox
  , module Reanimate.Svg.Unuse
  ) where

import           Control.Lens                 ((%~), (&), (.~), (^.), (?~))
import           Control.Monad.State
import           Graphics.SvgTree             hiding (height, line, path, use,
                                               width)
import           Linear.V2                    hiding (angle)
import           Reanimate.Constants
import           Reanimate.Animation (SVG)
import           Reanimate.Svg.Constructors
import           Reanimate.Svg.LineCommand
import           Reanimate.Svg.BoundingBox
import           Reanimate.Svg.Unuse
import qualified Reanimate.Transform          as Transform

lowerTransformations :: Tree -> Tree
lowerTransformations = worker Transform.identity
  where
    updLineCmd m cmd =
      case cmd of
        LineMove p    -> LineMove $ Transform.transformPoint m p
        -- LineDraw p -> LineDraw $ Transform.transformPoint m p
        LineBezier ps -> LineBezier $ map (Transform.transformPoint m) ps
        LineEnd p     -> LineEnd $ Transform.transformPoint m p
    updPath m = lineToPath . map (updLineCmd m) . toLineCommands
    worker m t =
      let m' = m * Transform.mkMatrix (t^.transform) in
      case t of
        PathTree path -> PathTree $
          path & pathDefinition %~ updPath m'
               & transform .~ Nothing
        GroupTree g -> GroupTree $
          g & groupChildren %~ map (worker m')
            & transform .~ Nothing
        ClipPathTree{} -> t
        _ -> mkGroup [t] & transform ?~ [ Transform.toTransformation m ]

lowerIds :: Tree -> Tree
lowerIds = mapTree worker
  where
    worker t@GroupTree{} = t & attrId .~ Nothing
    worker t@PathTree{}  = t & attrId .~ Nothing
    worker t             = t

simplify :: Tree -> Tree
simplify root =
  case worker root of
    []  -> None
    [x] -> x
    xs  -> mkGroup xs
  where
    worker None = []
    worker (DefinitionTree d) =
      concatMap dropNulls
      [DefinitionTree $ d & groupChildren %~ concatMap worker]
    worker (GroupTree g)
      | g ^. drawAttributes == defaultSvg =
        concatMap dropNulls $
        concatMap worker (g^.groupChildren)
      | otherwise =
        dropNulls $
        GroupTree $ g & groupChildren %~ concatMap worker
    worker t = dropNulls t

    dropNulls None = []
    dropNulls (DefinitionTree d)
      | null (d^.groupChildren) = []
    dropNulls (GroupTree g)
      | null (g^.groupChildren) = []
    dropNulls t = [t]

removeGroups :: Tree -> [Tree]
removeGroups = worker defaultSvg
  where
    worker _attr None = []
    worker _attr (DefinitionTree d) =
      concatMap dropNulls $
      [DefinitionTree $ d & groupChildren %~ concatMap (worker defaultSvg)]
    worker attr (GroupTree g)
      | g ^. drawAttributes == defaultSvg =
        concatMap dropNulls $
        concatMap (worker attr) (g^.groupChildren)
      | otherwise =
        concatMap (worker (attr <> g ^. drawAttributes)) (g^.groupChildren)
    worker attr t = dropNulls (t & drawAttributes .~ attr)

    dropNulls None = []
    dropNulls (DefinitionTree d)
      | null (d^.groupChildren) = []
    dropNulls (GroupTree g)
      | null (g^.groupChildren) = []
    dropNulls t = [t]

extractPath :: Tree -> [PathCommand]
extractPath = worker . simplify . lowerTransformations . pathify
  where
    worker (GroupTree g) = concatMap worker (g^.groupChildren)
    worker (PathTree p)  = p^.pathDefinition
    worker _             = []

withSubglyphs :: [Int] -> (Tree -> Tree) -> Tree -> Tree
withSubglyphs target fn = \t -> evalState (worker t) 0
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
    handleGlyph svg = do
      n <- get <* modify (+1)
      if n `elem` target
        then return $ fn svg
        else return svg

splitGlyphs :: [Int] -> Tree -> (Tree, Tree)
splitGlyphs target = \t ->
    let (_, l, r) = execState (worker id t) (0, [], [])
    in (mkGroup l, mkGroup r)
  where
    handleGlyph :: Tree -> State (Int, [Tree], [Tree]) ()
    handleGlyph t = do
      (n, l, r) <- get
      if n `elem` target
        then put (n+1, l, t:r)
        else put (n+1, t:l, r)
    worker :: (Tree -> Tree) -> Tree -> State (Int, [Tree], [Tree]) ()
    worker acc t =
      case t of
        GroupTree g -> do
          let acc' sub = acc (GroupTree $ g & groupChildren .~ [sub])
          mapM_ (worker acc') (g ^. groupChildren)
        PathTree{} -> handleGlyph $ acc t
        CircleTree{} -> handleGlyph $ acc t
        PolyLineTree{} -> handleGlyph $ acc t
        PolygonTree{} -> handleGlyph $ acc t
        EllipseTree{} -> handleGlyph $ acc t
        LineTree{} -> handleGlyph $ acc t
        RectangleTree{} -> handleGlyph $ acc t
        DefinitionTree{} -> return ()
        _ ->
          modify $ \(n, l, r) -> (n, acc t:l, r)
{-
<g transform="translate(10,10)">
  <g transform="scale(2)">
    <circle/>
  </g>
  <g transform="scale(0.5)">
    <rect/>
  </g>
</g>

[ (\svg -> <g transform="translate(10,10)"><g transform="scale(2)">svg</g></g>, <circle/>)
, (\svg -> <g transform="translate(10,10)"><g transform="scale(0.5)">svg</g></g>, <rect/>)]
-}
svgGlyphs :: Tree -> [(Tree -> Tree, DrawAttributes, Tree)]
svgGlyphs = worker id defaultSvg
  where
    worker acc attr =
      \case
        GroupTree g ->
          let acc' sub = acc (GroupTree $ g & groupChildren .~ [sub])
              attr' = (g^.drawAttributes) `mappend` attr
          in concatMap (worker acc' attr') (g ^. groupChildren)
        t -> [(acc, attr, t)]

{-| Convert primitive SVG shapes (like those created by 'mkCircle', 'mkRect', 'mkLine' or
    'mkEllipse') into SVG path. This can be useful for creating animations of these shapes being
    drawn progressively with 'partialSvg'.

    Example:

    > pathifyExample :: Animation
    > pathifyExample = animate $ \t -> gridLayout
    >     [ [ partialSvg t $ pathify $ mkCircle 1
    >       , partialSvg t $ pathify $ mkRect 2 2
    >       ]
    >     , [ partialSvg t $ pathify $ mkEllipse 1 0.5
    >       , partialSvg t $ pathify $ mkLine (-1, -1) (1, 1)
    >       ]
    >     ]

    <<docs/gifs/doc_pathify.gif>>
 -}
pathify :: Tree -> Tree
pathify = mapTree worker
  where
    worker =
      \case
        RectangleTree rect | Just (x,y,w,h) <- unpackRect rect ->
          PathTree $ defaultSvg
            & drawAttributes .~ rect ^. drawAttributes
            & strokeLineCap .~ pure CapSquare
            & pathDefinition .~
              [MoveTo OriginAbsolute [V2 x y]
              ,HorizontalTo OriginRelative [w]
              ,VerticalTo OriginRelative [h]
              ,HorizontalTo OriginRelative [-w]
              ,EndPath ]
        LineTree line | Just (x1,y1, x2, y2) <- unpackLine line ->
          PathTree $ defaultSvg
            & drawAttributes .~ line ^. drawAttributes
            & pathDefinition .~
              [MoveTo OriginAbsolute [V2 x1 y1]
              ,LineTo OriginAbsolute [V2 x2 y2] ]
        CircleTree circ | Just (x, y, r) <- unpackCircle circ ->
          PathTree $ defaultSvg
            & drawAttributes .~ circ ^. drawAttributes
            & pathDefinition .~
              [MoveTo OriginAbsolute [V2 (x-r) y]
              ,EllipticalArc OriginRelative [(r, r, 0,True,False,V2 (r*2) 0)
                                            ,(r, r, 0,True,False,V2 (-r*2) 0)]]
        PolyLineTree pl ->
          let points = pl ^. polyLinePoints
          in PathTree $ defaultSvg
               & drawAttributes .~ pl ^. drawAttributes
               & pathDefinition .~ pointsToPathCommands points
        PolygonTree pg ->
          let points = pg ^. polygonPoints
          in PathTree $ defaultSvg
               & drawAttributes .~ pg ^. drawAttributes
               -- Polygon automatically connects the last point to the first. For path we must do
               -- it explicitly
               & pathDefinition .~ (pointsToPathCommands points ++ [EndPath])
        EllipseTree elip | Just (cx,cy,rx,ry) <- unpackEllipse elip ->
          PathTree $ defaultSvg
             & drawAttributes .~ elip ^. drawAttributes
             & pathDefinition .~
               [ MoveTo OriginAbsolute [V2 (cx-rx) cy]
               , EllipticalArc OriginRelative [(rx, ry, 0,True,False,V2 (rx*2) 0)
                                              ,(rx, ry, 0,True,False,V2 (-rx*2) 0)]]
        t -> t
    unpackCircle circ = do
      let (x,y) = circ ^. circleCenter
      liftM3 (,,) (unpackNumber x) (unpackNumber y) (unpackNumber $ circ ^. circleRadius)
    unpackEllipse elip = do
      let (x,y) = elip ^. ellipseCenter
      liftM4 (,,,) (unpackNumber x) (unpackNumber y) (unpackNumber $ elip ^. ellipseXRadius)
                  (unpackNumber $ elip ^. ellipseYRadius)
    unpackLine line = do
      let (x1,y1) = line ^. linePoint1
          (x2,y2) = line ^. linePoint2
      liftM4 (,,,) (unpackNumber x1) (unpackNumber y1) (unpackNumber x2) (unpackNumber y2)
    unpackRect rect = do
      let (x', y') = rect ^. rectUpperLeftCorner
      x <- unpackNumber x'
      y <- unpackNumber y'
      w <- unpackNumber =<< rect ^. rectWidth
      h <- unpackNumber =<< rect ^. rectHeight
      return (x,y,w,h)
    pointsToPathCommands points = case points of
      [] -> []
      (p:ps) -> [ MoveTo OriginAbsolute [p]
                , LineTo OriginAbsolute ps ]
    unpackNumber n =
      case toUserUnit defaultDPI n of
        Num d -> Just d
        _     -> Nothing

mapSvgPaths :: ([PathCommand] -> [PathCommand]) -> SVG -> SVG
mapSvgPaths fn = mapTree worker
  where
    worker =
      \case
        PathTree path -> PathTree $
          path & pathDefinition %~ fn
        t -> t

mapSvgLines :: ([LineCommand] -> [LineCommand]) -> SVG -> SVG
mapSvgLines fn = mapSvgPaths (lineToPath . fn . toLineCommands)

-- Only maps points in paths
mapSvgPoints :: (RPoint -> RPoint) -> SVG -> SVG
mapSvgPoints fn = mapSvgLines (map worker)
  where
    worker (LineMove p) = LineMove (fn p)
    worker (LineBezier ps) = LineBezier (map fn ps)
    worker (LineEnd p) = LineEnd (fn p)

svgPointsToRadians :: SVG -> SVG
svgPointsToRadians = mapSvgPoints worker
  where
    worker (V2 x y) = V2 (x/180*pi) (y/180*pi)
