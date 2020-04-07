module Reanimate.Morph.Rotational
  ( rotationalTrajectory
  , polygonOrigin
  ) where

import qualified Data.Vector            as V
import           Linear.Vector
import           Linear.V2
import           Linear.Metric

import           Reanimate.Signal
import           Reanimate.Morph.Common
import           Reanimate.Math.Common

type Origin = (Double, Double)

rotationalTrajectory :: Origin -> Trajectory
rotationalTrajectory origin (src,dst) =
    \t ->
      let thisOrigin = lerp t dstOrigin srcOrigin in
      V.generate (length src) $ \i ->
        let len = fromToS (srcLengths V.! i) (dstLengths V.! i) t
            ang = lerpAngle (srcAngles V.! i) (dstAngles V.! i) t
        in realToFrac <$> (thisOrigin + V2 (cos ang * len) (sin ang * len))
  where
    srcOrigin = polygonOrigin src origin
    dstOrigin = polygonOrigin dst origin
    srcLengths :: V.Vector Double
    srcLengths = V.map (distance srcOrigin . fmap realToFrac) src
    dstLengths = V.map (distance dstOrigin . fmap realToFrac) dst
    srcAngles = V.map (originAngle srcOrigin . fmap realToFrac) src
    dstAngles = V.map (originAngle dstOrigin . fmap realToFrac) dst

    originAngle o v = lineAngle (o + V2 1 0) o v

polygonOrigin :: Polygon -> Origin -> V2 Double
polygonOrigin poly (originX, originY) =
  case polygonBox poly of
    (polyX, polyY, polyWidth, polyHeight) ->
      V2 (realToFrac polyX + realToFrac polyWidth * originX)
         (realToFrac polyY + realToFrac polyHeight * originY)


lerpAngle :: Double -> Double -> Double -> Double
lerpAngle fromAng toAng t
  | abs (fromAng - (toAng+2*pi)) < abs (fromAng - toAng) = (1-t)*fromAng + t*(toAng+2*pi)
  | abs (fromAng - (toAng-2*pi)) < abs (fromAng - toAng) = (1-t)*fromAng + t*(toAng-2*pi)
  | otherwise = (1-t)*fromAng + t*toAng

-- Angle from a through b to c.
lineAngle :: V2 Double -> V2 Double -> V2 Double -> Double
lineAngle a b c = angle' (a-b) (c-b)

angle' :: V2 Double -> V2 Double -> Double
angle' a b = atan2 (crossZ a b) (dot a b)
