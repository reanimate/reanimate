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

import           Control.Arrow ((***))
import           Control.Lens ((^.))
import           Data.List
import           Data.Maybe (mapMaybe)
import qualified Data.Vector.Unboxed as V
import qualified Geom2D.CubicBezier.Linear as Bezier
import           Graphics.SvgTree
import           Linear.V2 hiding (angle)
import           Linear.Vector
import           Reanimate.Constants
import           Reanimate.Svg.LineCommand
import qualified Reanimate.Transform as Transform

-- | Return bounding box of SVG tree.
--  The four numbers returned are (minimal X-coordinate, minimal Y-coordinate, width, height)
--
--  Note: Bounding boxes are computed on a best-effort basis and will not work
--        in all cases. The only supported SVG nodes are: path, circle, polyline,
--        ellipse, line, rectangle, image. All other nodes return (0,0,0,0).
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
      None            -> []
      UseTree{}       -> []
      GroupTree g     -> concatMap svgBoundingPoints (g^.groupChildren)
      SymbolTree g    -> concatMap svgBoundingPoints (g^.groupChildren)
      FilterTree{}    -> []
      DefinitionTree{} -> []
      PathTree p      -> linePoints $ toLineCommands (p^.pathDefinition)
      CircleTree c    -> circleBoundingPoints c
      PolyLineTree pl -> pl ^. polyLinePoints
      EllipseTree e   -> ellipseBoundingPoints e
      LineTree line   -> map pointToRPoint [line^.linePoint1, line^.linePoint2]
      RectangleTree rect ->
        case pointToRPoint (rect^.rectUpperLeftCorner) of
          V2 x y -> V2 x y :
            case mapTuple (fmap $ toUserUnit defaultDPI) (rect^.rectWidth, rect^.rectHeight) of
              (Just (Num w), Just (Num h)) -> [V2 (x+w) (y+h)]
              _                            -> []
      TextTree{}      -> []
      ImageTree img   ->
        case (img^.imageCornerUpperLeft, img^.imageWidth, img^.imageHeight) of
          ((Num x, Num y), Num w, Num h) ->
            [V2 x y, V2 (x+w) (y+h)]
          _ -> []
      MeshGradientTree{} -> []
      _ -> []
  where
    m = Transform.mkMatrix (t^.transform)
    mapTuple f = f *** f
    pointToRPoint p =
      case mapTuple (toUserUnit defaultDPI) p of
        (Num x, Num y) -> V2 x y
        _              -> error "Reanimate.Svg.svgBoundingPoints: Unrecognized number format."

    circleBoundingPoints circ =
      let (xnum, ynum) = circ ^. circleCenter
          rnum = circ ^. circleRadius
      in case mapMaybe unpackNumber [xnum, ynum, rnum] of
        [x, y, r] -> [ V2 (x + r * cos angle) (y + r * sin angle) | angle <- [0, pi/10 .. 2 * pi]]
        _  -> []

    ellipseBoundingPoints e =
      let (xnum,ynum) = e ^. ellipseCenter
          xrnum = e ^. ellipseXRadius
          yrnum = e ^. ellipseYRadius
      in case mapMaybe unpackNumber [xnum, ynum, xrnum, yrnum] of
        [x,y,xr,yr] -> [V2 (x + xr * cos angle) (y + yr * sin angle) | angle <- [0, pi/10 .. 2 * pi]]
        _ -> []

    unpackNumber n =
      case toUserUnit defaultDPI n of
        Num d -> Just d
        _     -> Nothing
