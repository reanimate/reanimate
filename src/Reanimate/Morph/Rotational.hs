{-|
Copyright   : Written by David Himmelstrup
License     : Unlicense
Maintainer  : lemmih@gmail.com
Stability   : experimental
Portability : POSIX
-}
module Reanimate.Morph.Rotational
  ( Origin
  , rotationalTrajectory
  , polygonOrigin
  ) where

import qualified Data.Vector            as V
import           Linear.Vector
import           Linear.V2
import           Linear.Metric

import           Reanimate.Ease
import           Reanimate.Morph.Common
import           Reanimate.Math.Polygon

-- | Rotational origin relative to polygon center.
--   (0.5, 0.5) is center of polygon. Top right is (1,1) and
--   bottom left is (0,0)
type Origin = (Double, Double)

-- | Interpolation by rotating around an origin point.
--
--   Example:
--
-- @
-- 'Reanimate.playThenReverseA' $ 'Reanimate.pauseAround' 0.5 0.5 $ 'Reanimate.mkAnimation' 3 $ \\t ->
--   'Reanimate.withStrokeLineJoin' 'Graphics.SvgTree.JoinRound' $
--   let src = 'Reanimate.scale' 8 $ 'Reanimate.center' $ 'Reanimate.LaTeX.latex' \"X\"
--       dst = 'Reanimate.scale' 8 $ 'Reanimate.center' $ 'Reanimate.LaTeX.latex' \"H\"
--   in 'morph' 'Reanimate.Morph.Linear.linear'{'morphTrajectory'='rotationalTrajectory' (0.5,0.5)} src dst t
-- @
--
--   <<docs/gifs/doc_rotationalTrajectory.gif>>
rotationalTrajectory :: Origin -> Trajectory
rotationalTrajectory origin (src,dst) =
    \t ->
      let thisOrigin = lerp t dstOrigin srcOrigin in
      mkPolygon $
      V.generate (pSize src) $ \i ->
        let len = fromToS (srcLengths V.! i) (dstLengths V.! i) t
            ang = lerpAngle (srcAngles V.! i) (dstAngles V.! i) t
        in realToFrac <$> (thisOrigin + V2 (cos ang * len) (sin ang * len))
  where
    srcOrigin = polygonOrigin src origin
    dstOrigin = polygonOrigin dst origin
    srcLengths :: V.Vector Double
    srcLengths = V.map (distance srcOrigin . fmap realToFrac) $ polygonPoints src
    dstLengths = V.map (distance dstOrigin . fmap realToFrac) $ polygonPoints dst
    srcAngles = V.map (originAngle srcOrigin . fmap realToFrac) $ polygonPoints src
    dstAngles = V.map (originAngle dstOrigin . fmap realToFrac) $ polygonPoints dst

    originAngle o = lineAngle (o + V2 1 0) o

-- | Compute the absolute position of rotational origin point in polygon.
polygonOrigin :: Polygon -> Origin -> V2 Double
polygonOrigin poly (originX, originY) =
  case pBoundingBox poly of
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
