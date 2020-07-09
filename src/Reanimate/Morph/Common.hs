{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE UnicodeSyntax   #-}
module Reanimate.Morph.Common
  ( PointCorrespondence
  , Trajectory
  , ObjectCorrespondence
  , Morph(..)
  , morph
  , splitObjectCorrespondence
  , dupObjectCorrespondence
  , genesisObjectCorrespondence
  , toShapes
  , normalizePolygons
  , annotatePolygons
  , unsafeSVGToPolygon
  ) where

import           Control.Lens
import qualified Data.Vector            as V
import           Graphics.SvgTree       (DrawAttributes, Texture (..),
                                         drawAttributes, fillColor, fillOpacity,
                                         groupOpacity, strokeColor,
                                         strokeOpacity)
import           Linear.V2
import           Reanimate.Animation
import           Reanimate.ColorComponents
import           Reanimate.Math.EarClip
import           Reanimate.Math.Polygon (Polygon, mkPolygon, pAddPoints,
                                         pCentroid, pRing, pSize, pdualPolygons,
                                         polygonPoints)
import           Reanimate.Math.SSSP
import           Reanimate.PolyShape
import           Reanimate.Ease
import           Reanimate.Svg

-- Correspondence
-- Trajectory
-- Color interpolation
-- Polygon holes
-- Polygon splitting

-- Graphical polygon? FIXME: Come up with a better name.
type GPolygon = (DrawAttributes, Polygon)

type PointCorrespondence = Polygon → Polygon → (Polygon, Polygon)
type Trajectory = (Polygon, Polygon) → (Double → Polygon)
type ObjectCorrespondence = [GPolygon] → [GPolygon] → [(GPolygon, GPolygon)]

data Morph = Morph
  { morphTolerance            :: Double
  , morphColorComponents      :: ColorComponents
  , morphPointCorrespondence  :: PointCorrespondence
  , morphTrajectory           :: Trajectory
  , morphObjectCorrespondence :: ObjectCorrespondence
  }

morph :: Morph -> SVG -> SVG -> Double -> SVG
morph Morph{..} src dst = \t ->
  case t of
    -- 0 -> lowerTransformations src
    -- 1 -> lowerTransformations dst
    _ -> mkGroup
          [ render (genPoints t)
              & drawAttributes .~ genAttrs t
          | (genAttrs, genPoints) <- gens
          ]
  where
    render p = mkLinePathClosed
        [ (x,y) | V2 x y <- map (fmap realToFrac) $ V.toList $ polygonPoints p ]
    srcShapes = toShapes morphTolerance src
    dstShapes = toShapes morphTolerance dst
    pairs = morphObjectCorrespondence srcShapes dstShapes
    gens =
      [ (interpolateAttrs morphColorComponents srcAttr dstAttr, morphTrajectory arranged)
      | ((srcAttr, srcPoly'), (dstAttr, dstPoly')) <- pairs
      , let arranged = morphPointCorrespondence srcPoly' dstPoly'
      ]

normalizePolygons :: Polygon -> Polygon -> (Polygon, Polygon)
normalizePolygons src dst =
    (pAddPoints (max 0 $ dstN-srcN) src
    ,pAddPoints (max 0 $ srcN-dstN) dst)
  where
    srcN = pSize src
    dstN = pSize dst

interpolateAttrs :: ColorComponents -> DrawAttributes -> DrawAttributes -> Double -> DrawAttributes
interpolateAttrs colorComps src dst t =
    src & fillColor .~ (interpColor <$> src^.fillColor <*> dst^.fillColor)
        & strokeColor .~ (interpColor <$> src^.strokeColor <*> dst^.strokeColor)
        & fillOpacity .~ (interpOpacity <$> src^.fillOpacity <*> dst^.fillOpacity)
        & groupOpacity .~ (interpOpacity <$> src^.groupOpacity <*> dst^.groupOpacity)
        & strokeOpacity .~ (interpOpacity <$> src^.strokeOpacity <*> dst^.strokeOpacity)
  where
    interpColor (ColorRef a) (ColorRef b) =
      ColorRef $ interpolateRGBA8 colorComps a b t
    -- interpolateColor (ColorRef a) FillNone = ColorRef a
    interpColor a _ = a
    interpOpacity a b = realToFrac (fromToS (realToFrac a) (realToFrac b) t)

genesisObjectCorrespondence :: ObjectCorrespondence
genesisObjectCorrespondence left right =
  case (left, right) of
    ([] , []) -> []
    ([], (y1,y2):ys) ->
      ((y1,y2), (y1, emptyFrom y2 y2)) : genesisObjectCorrespondence [] ys
    ((x1,x2):xs, []) ->
      ((x1,x2), (x1, emptyFrom x2 x2)) : genesisObjectCorrespondence xs []
    (x:xs, y:ys) ->
      (x,y) : genesisObjectCorrespondence xs ys
  where
    emptyFrom a b = mkPolygon $ V.map (const $ pCentroid a) (polygonPoints b)

dupObjectCorrespondence :: ObjectCorrespondence
dupObjectCorrespondence left right =
  case (left, right) of
    (_, []) -> []
    ([], _) -> []
    ([x], [y]) ->
      [(x,y)]
    ([(x1,x2)], yShapes) ->
      let x2s = replicate (length yShapes) x2
      in dupObjectCorrespondence (map (x1,) x2s) yShapes
    (xShapes, [(y1,y2)]) ->
      let y2s = replicate (length xShapes) y2
      in dupObjectCorrespondence xShapes (map (y1,) y2s)
    (x:xs, y:ys) ->
      (x, y) : dupObjectCorrespondence xs ys

splitObjectCorrespondence :: ObjectCorrespondence
splitObjectCorrespondence = dupObjectCorrespondence
{- This code is broken. :(
splitObjectCorrespondence left right =
  case (left, right) of
    (_, []) -> []
    ([], _) -> []
    ([x], [y]) ->
      [(x,y)]
    ([(x1,x2)], yShapes) ->
      let x2s = splitPolygon (length yShapes) x2
      in splitObjectCorrespondence (map (x1,) x2s) yShapes
    (xShapes, [(y1,y2)]) ->
      let y2s = splitPolygon (length xShapes) y2
      in splitObjectCorrespondence xShapes (map (y1,) y2s)
    (x:xs, y:ys) ->
      (x,y) : splitObjectCorrespondence xs ys

splitPolygon :: Int -> Polygon -> [Polygon]
splitPolygon n polygon =
    let trig = earClip $ pRing polygon
        d = dual 0 trig
        pd = toPDual (pRing polygon) d
        reduced = pdualReduce (pRing polygon) pd n
        polygons = pdualPolygons polygon reduced
    in polygons
-}

-- joinPairs :: Correspondence -> [(DrawAttributes, PolyShape)] -> [(DrawAttributes, PolyShape)]
--           -> [(DrawAttributes, DrawAttributes, [(RPoint, RPoint)])]
-- joinPairs _ _ [] = []
-- joinPairs _ [] _ = []
-- joinPairs corr [(x1,x2)] [(y1,y2)] =
--   [(x1,y1, corr x2 y2)]
-- joinPairs corr [(x1,x2)] yShapes =
--   let x2s = splitPolyShape 0.001 (length yShapes) x2
--   in joinPairs corr (map (x1,) x2s) yShapes
-- joinPairs corr xShapes [(y1,y2)] =
--   let y2s = reverse $ splitPolyShape 0.001 (length xShapes) y2
--   in joinPairs corr xShapes (map (y1,) y2s)
-- joinPairs corr ((x1,x2):xs) ((y1,y2):ys) =
--   (x1,y1, corr x2 y2) : joinPairs corr xs ys
-- joinPairs _ _ _ = []

-- FIXME: sort by size, smallest to largest
toShapes :: Double -> SVG -> [(DrawAttributes, Polygon)]
toShapes tol src =
  [ (attrs, plToPolygon tol shape)
  | (_, attrs, glyph) <- svgGlyphs $ lowerTransformations $ pathify src
  , shape <- map mergePolyShapeHoles $ plGroupShapes $ svgToPolyShapes glyph
  ]

unsafeSVGToPolygon :: Double -> SVG -> Polygon
unsafeSVGToPolygon tol src = snd $ head $ toShapes tol src

annotatePolygons :: (Polygon -> SVG) -> SVG -> SVG
annotatePolygons fn svg = mkGroup
  [ fn poly & drawAttributes .~ attr
  | (attr, poly) <- toShapes 0.001 svg
  ]
