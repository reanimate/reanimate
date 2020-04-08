module Reanimate.Morph.LineBend
  ( lineBend
  , lineBendRaw
  ) where

import qualified Data.Vector            as V
import           Linear.Matrix          (det22)
import           Linear.Metric
import           Linear.V2
import           Linear.Vector
import           Reanimate.Math.Common
import           Reanimate.Morph.Common

lineBend :: Trajectory
lineBend = lineBend' True

lineBendRaw :: Trajectory
lineBendRaw = lineBend' False

-- Note: Returns polygon with n+1 elements.
lineBend' :: Bool -> Trajectory
lineBend' corrected (a,b) = \t ->
    let u = 1 - t
        -- phi = V.zipWith (\l r -> u*l + t*r) phi_a phi_b
        phi = V.zipWith (\l r -> lerpAngle l r t) phi_a phi_b
        -- alpha_zero = u * alpha_a_zero + t * alpha_b_zero
        alpha_zero = lerpAngle alpha_a_zero alpha_b_zero t
        alpha = V.scanl (+) alpha_zero phi
        bigE =
          V.sum $ V.zipWith (\diff alp -> squared diff * squared (cos alp)) lengths_diff alpha
        bigF =
          V.sum $ V.zipWith (\diff alp -> squared diff * sin alp * cos alp) lengths_diff alpha
        bigG =
          V.sum $ V.zipWith (\diff alp -> squared diff * squared (sin alp)) lengths_diff alpha
        bigU =
          2 * V.sum
          (V.zipWith3 (\len_a len_b alp -> (u*len_a + t*len_b) * cos alp) lengths_a lengths_b alpha)
        bigV =
          2 * V.sum
          (V.zipWith3 (\len_a len_b alp -> (u*len_a + t*len_b) * sin alp) lengths_a lengths_b alpha)
        lam1 = det22 (V2 (V2 bigU bigF) (V2 bigV bigG)) /
               det22 (V2 (V2 bigE bigF) (V2 bigF bigG))
        lam2 = det22 (V2 (V2 bigE bigU) (V2 bigF bigV)) /
               det22 (V2 (V2 bigE bigF) (V2 bigF bigG))
        s_vect = V.fromList
          [ -0.5 * squared (lengths_diff V.! n) *
            (lam1 * cos (alpha V.! n) +
             lam2 * sin (alpha V.! n))
          | n <- [0 .. length a-1]]
        lengths = V.zipWith3 (\l r s -> u*l + t*r + if corrected then s else 0) lengths_a lengths_b s_vect
        movingCenter :: V2 Double
        movingCenter = lerp t (realToFrac <$> polygonCentroid b) (realToFrac <$> polygonCentroid a)
        centerAng :: Double
        centerAng = lerpAngle centoid_angle_a centoid_angle_b t
        centerLen = u*centoid_len_a + t*centoid_len_b
        -- V2 x_zero y_zero = lerp t (realToFrac <$> b V.! 0) (realToFrac <$> a V.! 0)
        V2 x_zero y_zero = movingCenter + V2 (cos centerAng * centerLen) (sin centerAng * centerLen)
        x_modifiers = V.zipWith (*) lengths (V.map cos alpha)
        y_modifiers = V.zipWith (*) lengths (V.map sin alpha)
        xs = V.scanl (+) x_zero x_modifiers
        ys = V.scanl (+) y_zero y_modifiers
    in V.map (fmap realToFrac) $ V.zipWith V2 xs ys
  where
    squared x = x*x
    centoid_angle_a :: Double
    centoid_angle_a = lineAngle (polygonCentroid a + V2 1 0) (polygonCentroid a) (a V.! 0)
    centoid_angle_b = lineAngle (polygonCentroid b + V2 1 0) (polygonCentroid b) (b V.! 0)
    centoid_len_a :: Double
    centoid_len_a = realToFrac $ approxDist (polygonCentroid a) (a V.! 0)
    centoid_len_b = realToFrac $ approxDist (polygonCentroid b) (b V.! 0)
    alpha_a_zero = lineAngle (a V.! 0 + V2 1 0) (a V.! 0) (a V.! 1)
    alpha_b_zero = lineAngle (b V.! 0 + V2 1 0) (b V.! 0) (b V.! 1)
    lengths_a = computeLength a
    lengths_b = computeLength b
    lengths_diff' = V.zipWith (\l r -> abs (l-r)) lengths_a lengths_b
    lengths_tol = 0.0001 * V.maximum lengths_diff'
    lengths_diff = V.map (max lengths_tol) lengths_diff'
    phi_a = computePhi a
    phi_b = computePhi b

    computeLength :: Polygon -> V.Vector Double
    computeLength poly = V.fromList
      [ realToFrac $ approxDist this next
      | n <- [0 .. length poly-1]
      , let this = pAccess poly n
            next = pAccess poly (pNext a n)
      ]
    computePhi poly = V.fromList $
      [ negate $ angle' (next-this) ((prev-this) ^* (-1))
      | n <- [1 .. length poly-1]
      , let prev = pAccess poly (n-1)
            this = pAccess poly n
            next = pAccess poly (pNext a n)
      ]

lerpAngle :: Double -> Double -> (Double -> Double)
lerpAngle fromAng toAng t
  | abs (fromAng - (toAng+2*pi)) < abs (fromAng - toAng) = (1-t)*fromAng + t*(toAng+2*pi)
  | abs (fromAng - (toAng-2*pi)) < abs (fromAng - toAng) = (1-t)*fromAng + t*(toAng-2*pi)
  | otherwise = (1-t)*fromAng + t*toAng

-- Angle from a through b to c. Measured on the right-hand side, from 0 to tau
lineAngle :: V2 Rational -> V2 Rational -> V2 Rational -> Double
lineAngle a b c = angle' (a-b) (c-b)

angle' :: V2 Rational -> V2 Rational -> Double
angle' a b = atan2 (realToFrac $ crossZ a b) (realToFrac $ dot a b)
