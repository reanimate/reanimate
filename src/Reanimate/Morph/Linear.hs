module Reanimate.Morph.Linear
  ( linear, rawLinear
  ) where

import qualified Data.Vector            as V
import           Linear.Vector

import           Reanimate.Interpolate
import           Reanimate.Morph.Common
import           Reanimate.Math.Common

linear :: Morph
linear = rawLinear
  { morphPointCorrespondence  = closestLinearCorrespondence }

rawLinear :: Morph
rawLinear = Morph
  { morphTolerance            = 0.001
  , morphColorComponents      = labComponents
  , morphPointCorrespondence  = linearCorrespondence
  , morphTrajectory           = linearTrajectory
  , morphObjectCorrespondence = splitObjectCorrespondence }

linearCorrespondence :: PointCorrespondence
linearCorrespondence src dst = (src, dst)

closestLinearCorrespondence :: PointCorrespondence
closestLinearCorrespondence src dst =
    (src, worker dst (score dst) options)
  where
    worker bestP _bestPScore [] = bestP
    worker bestP bestPScore (x:xs) =
      let newScore = score x in
      if newScore < bestPScore
        then worker x newScore xs
        else worker bestP bestPScore xs
    options = cyclePolygons dst
    score = V.sum . V.zipWith approxDist src

linearTrajectory :: Trajectory
linearTrajectory (src,dst) = \t -> V.zipWith (lerp $ realToFrac t) dst src
