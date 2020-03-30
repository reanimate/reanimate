{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}
module Reanimate.Morph.Common
  ( PointCorrespondence
  , Trajectory
  , ObjectCorrespondence
  , Morph(..)
  , morph
  , splitObjectCorrespondence
  , dupObjectCorrespondence
  ) where

import           Control.Lens
import qualified Data.Vector            as V
import           Graphics.SvgTree       (DrawAttributes, Texture (..),
                                         drawAttributes, fillColor, fillOpacity,
                                         groupOpacity, strokeColor,
                                         strokeOpacity)
import           Linear.V2
import           Linear.Vector
import           Reanimate.Animation
import           Reanimate.Interpolate
import           Reanimate.Math.Common  (Polygon, approxDist, pAccess)
import           Reanimate.Math.EarClip
import           Reanimate.Math.SSSP
import           Reanimate.PolyShape
import           Reanimate.Signal
import           Reanimate.Svg

-- Correspondence
-- Trajectory
-- Color interpolation
-- Polygon holes
-- Polygon splitting

type PointCorrespondence = Polygon -> Polygon -> (Polygon, Polygon)
type Trajectory = (Polygon, Polygon) -> (Double -> Polygon)
type ObjectCorrespondence =
  [(DrawAttributes, Polygon)] -> [(DrawAttributes, Polygon)] ->
  [(DrawAttributes, DrawAttributes, Polygon, Polygon)]

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
    0 -> src
    1 -> dst
    _ -> mkGroup
          [ (render $ genPoints t)
              & drawAttributes .~ genAttrs t
          | (genAttrs, genPoints) <- gens
          ]
  where
    render p =
      mkLinePathClosed
        [ (x,y) | V2 x y <- map (fmap realToFrac) $ V.toList p  ++ [pAccess p 0] ]
    srcShapes = toShapes morphTolerance src
    dstShapes = toShapes morphTolerance dst
    pairs = morphObjectCorrespondence srcShapes dstShapes
    gens =
      [ (interpolateAttrs morphColorComponents srcAttr dstAttr, morphTrajectory arranged)
      | (srcAttr, dstAttr, srcPoly', dstPoly') <- pairs
      , let arranged = applyPointCorrespondence morphPointCorrespondence srcPoly' dstPoly'
      ]

applyPointCorrespondence :: PointCorrespondence -> Polygon -> Polygon -> (Polygon, Polygon)
applyPointCorrespondence fn src dst =
    fn (addPoints (max 0 $ dstN-srcN) src)
       (addPoints (max 0 $ srcN-dstN) dst)
  where
    srcN = length src
    dstN = length dst

addPoints :: Int -> Polygon -> Polygon
addPoints n p = V.fromList $ worker n 0 (V.toList p ++ [pAccess p 0])
  where
    worker 0 _ rest = init rest
    worker i acc (x:y:xs) =
      let xy = approxDist x y in
      if acc + xy > limit
        then x : worker (i-1) 0 (lerp ((limit-acc)/xy) y x : y:xs)
        else x : worker i (acc+xy) (y:xs)
    worker _ _ _ = []
    len = V.sum (V.zipWith approxDist p (V.tail p)) + approxDist (V.last p) (V.head p)
    limit = len / fromIntegral (n+1)


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

dupObjectCorrespondence :: ObjectCorrespondence
dupObjectCorrespondence left right =
  case (left, right) of
    (_, []) -> []
    ([], _) -> []
    ([(x1,x2)], [(y1,y2)]) ->
      [(x1,y1,x2,y2)]
    ([(x1,x2)], yShapes) ->
      let x2s = replicate (length yShapes) x2
      in dupObjectCorrespondence (map (x1,) x2s) yShapes
    (xShapes, [(y1,y2)]) ->
      let y2s = replicate (length xShapes) y2
      in dupObjectCorrespondence xShapes (map (y1,) y2s)
    ((x1,x2):xs, (y1,y2):ys) ->
      (x1, y1, x2, y2) : dupObjectCorrespondence xs ys

splitObjectCorrespondence :: ObjectCorrespondence
splitObjectCorrespondence left right =
  case (left, right) of
    (_, []) -> []
    ([], _) -> []
    ([(x1,x2)], [(y1,y2)]) ->
      [(x1,y1,x2,y2)]
    ([(x1,x2)], yShapes) ->
      let x2s = splitPolygon (length yShapes) x2
      in splitObjectCorrespondence (map (x1,) x2s) yShapes
    (xShapes, [(y1,y2)]) ->
      let y2s = splitPolygon (length xShapes) y2
      in splitObjectCorrespondence xShapes (map (y1,) y2s)
    ((x1,x2):xs, (y1,y2):ys) ->
      (x1, y1, x2, y2) : splitObjectCorrespondence xs ys

splitPolygon :: Int -> Polygon -> [Polygon]
splitPolygon n polygon =
    let trig = earClip polygon
        d = dual trig
        pd = toPDual polygon d
        reduced = pdualReduce polygon pd n
        polygons = pdualPolygons polygon reduced
    in polygons


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
  | (_, attrs, glyph) <- svgGlyphs $ lowerTransformations $ pathify $ src
  , shape <- map mergePolyShapeHoles $ plGroupShapes $ svgToPolyShapes glyph
  ]
