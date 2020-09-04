{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE UnicodeSyntax   #-}
{-|
Copyright   : Written by David Himmelstrup
License     : Unlicense
Maintainer  : lemmih@gmail.com
Stability   : experimental
Portability : POSIX
-}
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
import qualified Data.Vector               as V
import           Graphics.SvgTree          (DrawAttributes, Texture (..),
                                            drawAttributes, fillColor,
                                            fillOpacity, groupOpacity,
                                            strokeColor, strokeOpacity)
import           Linear.V2
import           Reanimate.Animation
import           Reanimate.ColorComponents
import           Reanimate.Ease
import           Reanimate.Math.Polygon    (APolygon, Epsilon, Polygon,
                                            mkPolygon, pAddPoints, pCentroid,
                                            pCutEqual, pSize, polygonPoints)
import           Reanimate.PolyShape
import           Reanimate.Svg

-- import Debug.Trace

-- Correspondence
-- Trajectory
-- Color interpolation
-- Polygon holes
-- Polygon splitting

-- Graphical polygon? FIXME: Come up with a better name.
type GPolygon = (DrawAttributes, Polygon)

-- | Method determining how points in the source polygon align with
--   points in the target polygon.
type PointCorrespondence = Polygon → Polygon → (Polygon, Polygon)

-- | Method for interpolating between two aligned polygons.
type Trajectory = (Polygon, Polygon) → (Double → Polygon)

-- | Method for pairing sets of polygons.
type ObjectCorrespondence = [GPolygon] → [GPolygon] → [(GPolygon, GPolygon)]

-- | Morphing strategy
data Morph = Morph
  { morphTolerance            :: Double
    -- ^ Morphing curves is not always possible and
    --   sometimes shapes are reduced to polygons or meta-curves.
    --   This parameter determined the accuracy of this transformation.
  , morphColorComponents      :: ColorComponents
    -- ^ Color components used for color interpolation. LAB is usually
    --   the best option here.
  , morphPointCorrespondence  :: PointCorrespondence
    -- ^ Desired point-correspondence algorithm.
  , morphTrajectory           :: Trajectory
    -- ^ Desired interpolation algorithm.
  , morphObjectCorrespondence :: ObjectCorrespondence
    -- ^ Desired object-correspondence algorithm.
  }

{-# INLINE morph #-}
-- | Apply morphing strategy to interpolate between two SVG images.
morph :: Morph -> SVG -> SVG -> Double -> SVG
morph Morph{..} src dst = \t ->
  case t of
    0 -> lowerTransformations src
    1 -> lowerTransformations dst
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

-- | Add points to each polygon such that they end up with same size.
normalizePolygons :: (Real a, Fractional a, Epsilon a) => APolygon a -> APolygon a -> (APolygon a, APolygon a)
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

-- | Object-correspondence algorithm that spawn objects as necessary.
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

-- | Object-correspondence algorithm that duplicate objects as necessary.
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

-- | Object-correspondence algorithm that splits objects in smaller pieces
--   as necessary.
splitObjectCorrespondence :: ObjectCorrespondence
-- splitObjectCorrespondence = dupObjectCorrespondence
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
splitPolygon 1 p = [p]
splitPolygon n p =
  let (a,b) = pCutEqual p
  in splitPolygon (n`div`2) a ++ splitPolygon ((n+1)`div`2) b

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
-- | Extract shapes and their graphical attributes from an SVG node.
toShapes :: Double -> SVG -> [(DrawAttributes, Polygon)]
toShapes tol src =
  [ (attrs, plToPolygon tol shape)
  | (_, attrs, glyph) <- svgGlyphs $ lowerTransformations $ pathify src
  , shape <- map mergePolyShapeHoles $ plGroupShapes $ svgToPolyShapes glyph
  ]

-- | Extract the first polygon in an SVG node. Will fail if there
--   are no acceptable shapes.
unsafeSVGToPolygon :: Double -> SVG -> Polygon
unsafeSVGToPolygon tol src = snd $ head $ toShapes tol src

-- | Map over each polygon in an SVG node.
annotatePolygons :: (Polygon -> SVG) -> SVG -> SVG
annotatePolygons fn svg = mkGroup
  [ fn poly & drawAttributes .~ attr
  | (attr, poly) <- toShapes 0.001 svg
  ]
