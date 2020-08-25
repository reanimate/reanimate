module Reanimate.Morph.Linear
  ( linear, rawLinear
  , linearCorrespondence
  , closestLinearCorrespondence
  , closestLinearCorrespondenceA
  , linearTrajectory
  ) where

import           Data.Hashable
import qualified Data.Vector            as V
import           Linear.Vector
import           Reanimate.ColorComponents
import           Reanimate.Math.Common
import           Reanimate.Math.Polygon
import           Reanimate.Morph.Cache
import           Reanimate.Morph.Common

linear :: Morph
linear = rawLinear
  { morphPointCorrespondence  =
      cachePointCorrespondence (hash ("closest"::String))
        closestLinearCorrespondence }

rawLinear :: Morph
rawLinear = Morph
  { morphTolerance            = 0.001
  , morphColorComponents      = labComponents
  , morphPointCorrespondence  = linearCorrespondence
  , morphTrajectory           = linearTrajectory
  , morphObjectCorrespondence = splitObjectCorrespondence }

linearCorrespondence :: PointCorrespondence
linearCorrespondence = normalizePolygons

closestLinearCorrespondence :: PointCorrespondence
closestLinearCorrespondence = closestLinearCorrespondenceA

closestLinearCorrespondenceA :: (Real a, Fractional a, Epsilon a) => APolygon a -> APolygon a -> (APolygon a, APolygon a)
closestLinearCorrespondenceA src' dst' =
    (src, worker dst (score dst) options)
  where
    (src, dst) = normalizePolygons src' dst'
    worker bestP _bestPScore [] = bestP
    worker bestP bestPScore (x:xs) =
      let newScore = score x in
      if newScore < bestPScore
        then worker x newScore xs
        else worker bestP bestPScore xs
    options = pCycles dst
    score p = sum
      [ -- approxDist (pAccess src n) (pAccess p n)
        distSquared (pAccess src n) (pAccess p n)
      | n <- [0 .. pSize src-1] ]

linearTrajectory :: Trajectory
linearTrajectory (src,dst)
  | pSize src == pSize dst = \t -> mkPolygon $
    V.zipWith (lerp $ realToFrac t) (polygonPoints dst) (polygonPoints src)
  | otherwise = error $ "Invalid lengths: " ++ show (pSize src, pSize dst)
