{-|
Copyright   : Written by David Himmelstrup
License     : Unlicense
Maintainer  : lemmih@gmail.com
Stability   : experimental
Portability : POSIX
-}
module Reanimate.Morph.Linear
  ( linear, rawLinear
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

-- | Linear interpolation strategy.
--
--   Example:
--
-- @
-- 'Reanimate.playThenReverseA' $ 'Reanimate.pauseAround' 0.5 0.5 $ 'Reanimate.mkAnimation' 3 $ \\t ->
--   'Reanimate.withStrokeLineJoin' 'Graphics.SvgTree.JoinRound' $
--   let src = 'Reanimate.scale' 8 $ 'Reanimate.center' $ 'Reanimate.LaTeX.latex' \"X\"
--       dst = 'Reanimate.scale' 8 $ 'Reanimate.center' $ 'Reanimate.LaTeX.latex' \"H\"
--   in 'morph' 'linear' src dst t
-- @
--
--   <<docs/gifs/doc_linear.gif>>
linear :: Morph
linear = rawLinear
  { morphPointCorrespondence  =
      cachePointCorrespondence (hash ("closest"::String))
        closestLinearCorrespondence }

-- | Linear interpolation strategy without realigning corners.
--   May give better results if the polygons are already aligned.
--   Usually gives worse results.
--
--   Example:
--
-- @
-- 'Reanimate.playThenReverseA' $ 'Reanimate.pauseAround' 0.5 0.5 $ 'Reanimate.mkAnimation' 3 $ \\t ->
--   'Reanimate.withStrokeLineJoin' 'Graphics.SvgTree.JoinRound' $
--   let src = 'Reanimate.scale' 8 $ 'Reanimate.center' $ 'Reanimate.LaTeX.latex' \"X\"
--       dst = 'Reanimate.scale' 8 $ 'Reanimate.center' $ 'Reanimate.LaTeX.latex' \"H\"
--   in 'morph' 'rawLinear' src dst t
-- @
--
--   <<docs/gifs/doc_rawLinear.gif>>
rawLinear :: Morph
rawLinear = Morph
  { morphTolerance            = 0.001
  , morphColorComponents      = labComponents
  , morphPointCorrespondence  = normalizePolygons
  , morphTrajectory           = linearTrajectory
  , morphObjectCorrespondence = splitObjectCorrespondence }

-- | Cycle polygons until the sum of the point trajectory path lengths
--   is smallest.
closestLinearCorrespondence :: PointCorrespondence
closestLinearCorrespondence = closestLinearCorrespondenceA

-- | Cycle polygons until the sum of the point trajectory path lengths
--   is smallest.
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

-- | Strategy for moving points in a linear (straight-line) trajectory.
linearTrajectory :: Trajectory
linearTrajectory (src,dst)
  | pSize src == pSize dst = \t -> mkPolygon $
    V.zipWith (lerp $ realToFrac t) (polygonPoints dst) (polygonPoints src)
  | otherwise = error $ "Invalid lengths: " ++ show (pSize src, pSize dst)
