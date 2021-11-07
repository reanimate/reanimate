{-|
  Bounding-boxes can be immensely useful for aligning objects
  but they are not part of the SVG specification and cannot be
  computed for all SVG nodes. In particular, you'll get bad results
  when asking for the bounding boxes of Text nodes (because fonts
  are difficult), clipped nodes, and filtered nodes.
-}
module Reanimate.Svg.BoundingBox
  ( boundingBox
  , svgHeight
  , svgWidth
  ) where

import           Control.Arrow             ((***))
import           Control.Lens              ((^.))
import           Data.List                 (foldl')
import           Data.Maybe                (mapMaybe)
import qualified Data.Vector.Unboxed       as V
import qualified Geom2D.CubicBezier.Linear as Bezier
import           Graphics.SvgTree
import           Linear.V2                 (V2 (V2))
import           Linear.Vector             (Additive (zero))
import           Reanimate.Constants       (defaultDPI)
import           Reanimate.Svg.LineCommand (LineCommand (..), toLineCommands)
import qualified Reanimate.Transform       as Transform

-- | Return bounding box of SVG tree.
--  The four numbers returned are (minimal X-coordinate, minimal Y-coordinate, width, height)
--
--  Note: Bounding boxes are computed on a best-effort basis and will not work
--        in all cases. The only supported SVG nodes are: path, circle, polyline,
--        ellipse, line, rectangle, image and svg. All other nodes return (0,0,0,0).
--        The box for the svg node is based on the document's width and height
--        (if both are present).
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

-- | Height of SVG node in local units (not pixels). Computed on best-effort basis
--   and will not give accurate results for all SVG nodes.
svgHeight :: Tree -> Double
svgHeight t = h
  where
    (_x, _y, _w, h) = boundingBox t

-- | Width of SVG node in local units (not pixels). Computed on best-effort basis
--   and will not give accurate results for all SVG nodes.
svgWidth :: Tree -> Double
svgWidth t = w
  where
    (_x, _y, w, _h) = boundingBox t

-- | Sampling of points in a line path.
linePoints :: [LineCommand] -> [RPoint]
linePoints = worker zero
  where
    worker _from [] = []
    worker from (x:xs) =
      case x of
        LineMove to     -> worker to xs
        -- LineDraw to     -> from:to:worker to xs
        LineBezier [p] ->
          p : worker p xs
        LineBezier ctrl -> -- approximation
          let bezier = Bezier.AnyBezier (V.fromList (from:ctrl))
          in [ Bezier.evalBezier bezier (recip chunks*i) | i <- [0..chunks]] ++
          worker (last ctrl) xs
        LineEnd p -> p : worker p xs
    chunks = 10

svgBoundingPoints :: Tree -> [RPoint]
svgBoundingPoints t = map (Transform.transformPoint m) $
    case t of
      None             -> []
      UseTree{}        -> []
      GroupTree g      -> concatMap svgBoundingPoints (g ^. groupChildren)
      SymbolTree g     -> concatMap svgBoundingPoints (g ^. groupChildren)
      FilterTree{}     -> []
      DefinitionTree{} -> []
      PathTree p       -> linePoints $ toLineCommands (p ^. pathDefinition)
      CircleTree c     -> circleBoundingPoints c
      PolyLineTree pl  -> pl ^. polyLinePoints
      EllipseTree e    -> ellipseBoundingPoints e
      LineTree l       -> map pointToRPoint [l ^. linePoint1, l ^. linePoint2]
      RectangleTree r  ->
        let p = pointToRPoint (r ^. rectUpperLeftCorner)
            mDims = (r ^. rectWidth, r ^. rectHeight)
        in  rectPoints p mDims
      TextTree{}       -> []
      ImageTree img    ->
        let p = pointToRPoint (img ^. imageCornerUpperLeft)
            dims = (img ^. imageWidth, img ^. imageHeight)
        in  rectPoints' p dims
      MeshGradientTree{} -> []
      SvgTree d        -> let mDims = (d ^. documentWidth, d ^. documentHeight)
                          in  rectPoints (V2 0 0) mDims
      _                -> []
  where
    m = Transform.mkMatrix (t ^. transform)
    mapTuple f = f *** f
    toUserUnit' = toUserUnit defaultDPI
    pointToRPoint p =
      case mapTuple toUserUnit' p of
        (Num x, Num y) -> V2 x y
        _              -> error "Reanimate.Svg.svgBoundingPoints: Unrecognized number format."

    circleBoundingPoints circ =
      let (xnum, ynum) = circ ^. circleCenter
          rnum = circ ^. circleRadius
      in case mapMaybe unpackNumber [xnum, ynum, rnum] of
        [x, y, r] -> ellipsePoints x y r r
        _         -> []

    ellipseBoundingPoints e =
      let (xnum,ynum) = e ^. ellipseCenter
          xrnum = e ^. ellipseXRadius
          yrnum = e ^. ellipseYRadius
      in case mapMaybe unpackNumber [xnum, ynum, xrnum, yrnum] of
        [x, y, xr, yr] -> ellipsePoints x y xr yr
        _              -> []

    ellipsePoints x y xr yr = [ V2 (x + xr * cos angle) (y + yr * sin angle)
                              | angle <- [0, pi/10 .. 2 * pi] ]

    rectPoints p mDims = case mDims of
                           (Just w, Just h) -> rectPoints' p (w, h)
                           _ -> [p]

    rectPoints' p@(V2 x y) dims =
      p : case mapTuple toUserUnit' dims of
            ((Num w), (Num h)) -> let (x', y') = (x + w, y + h)
                                  in  [V2 x' y, V2 x' y', V2 x y']
            _ -> []

    unpackNumber n =
      case toUserUnit' n of
        Num d -> Just d
        _     -> Nothing
