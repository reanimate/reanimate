module Reanimate.Morph.Linear
  ( linear, rawLinear
  , linearCorrespondence
  , closestLinearCorrespondence
  , linearTrajectory
  ) where

import           Data.Hashable
import qualified Data.Vector            as V
import           Linear.Vector
import           Reanimate.Interpolate
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
closestLinearCorrespondence src' dst' =
    (src, worker dst (score dst) options)
  where
    (src, dst) = normalizePolygons src' dst'
    worker bestP _bestPScore [] = bestP
    worker bestP bestPScore (x:xs) =
      let newScore = score x in
      if newScore < bestPScore
        then worker x newScore xs
        else worker bestP bestPScore xs
    options = cyclePolygons dst
    score = V.sum . V.zipWith approxDist (polygonPoints src) . polygonPoints

linearTrajectory :: Trajectory
linearTrajectory (src,dst)
  | polygonSize src == polygonSize dst = \t -> mkPolygon $
    V.zipWith (lerp $ realToFrac t) (polygonPoints dst) (polygonPoints src)
  | otherwise = error $ "Invalid lengths: " ++ show (polygonSize src, polygonSize dst)
