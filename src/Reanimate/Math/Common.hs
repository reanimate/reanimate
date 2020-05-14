module Reanimate.Math.Common
  ( -- * Ring
    Ring(..)
  , ringSize            -- :: Ring a -> Int
  , ringAccess          -- :: Ring a -> Int -> V2 a
  , ringClamp           -- :: Ring a -> Int -> Int
  , ringUnpack          -- :: Ring a -> Vector (V2 a)
  , ringPack            -- :: Vector (V2 a) -> Ring a
  , ringMap             -- :: (V2 a -> V2 b) -> Ring a -> Ring b
  , ringRayIntersect    -- :: Ring Rational -> (Int, Int) -> (Int,Int) -> Maybe (V2 Rational)
    -- * Math
  , area                -- :: Fractional a => V2 a -> V2 a -> V2 a -> a
  , area2X              -- :: Fractional a => V2 a -> V2 a -> V2 a -> a
  , epsilon             -- :: Fractional a => a
  , epsEq               -- :: (Ord a, Fractional a) => a -> a -> Bool
  , isLeftTurn          -- :: (Fractional a, Ord a) => V2 a -> V2 a -> V2 a -> Bool
  , isLeftTurnOrLinear  -- :: (Fractional a, Ord a) => V2 a -> V2 a -> V2 a -> Bool
  , isRightTurn         -- :: (Fractional a, Ord a) => V2 a -> V2 a -> V2 a -> Bool
  , isRightTurnOrLinear -- :: (Fractional a, Ord a) => V2 a -> V2 a -> V2 a -> Bool
  , direction           -- :: Fractional a => V2 a -> V2 a -> V2 a -> a
  , isInside            -- :: (Fractional a, Ord a) => V2 a -> V2 a -> V2 a -> V2 a -> Bool
  , barycentricCoords   -- :: Fractional a => V2 a -> V2 a -> V2 a -> V2 a -> (a, a, a)
  , rayIntersect        -- :: (Fractional a, Ord a) => (V2 a,V2 a) -> (V2 a,V2 a) -> Maybe (V2 a)
  , isBetween           -- :: (Ord a, Fractional a) => V2 a -> (V2 a, V2 a) -> Bool
  , lineIntersect       -- :: (Ord a, Fractional a) => (V2 a, V2 a) -> (V2 a, V2 a) -> Maybe (V2 a)
  , distSquared         -- :: (Fractional a) => V2 a -> V2 a -> a
  , approxDist          -- :: (Real a, Fractional a) => V2 a -> V2 a -> a
  , distance'           -- :: (Real a, Fractional a) => V2 a -> V2 a -> Double
  , triangleAngles      -- :: V2 Double -> V2 Double -> V2 Double -> (Double, Double, Double)
  ) where

import           Data.Vector    (Vector)
import qualified Data.Vector    as V
import           Linear.Matrix  (det33)
import           Linear.Metric
import           Linear.V2
import           Linear.V3
import           Linear.Vector

newtype Ring a = Ring (Vector (V2 a))

ringSize :: Ring a -> Int
ringSize (Ring v) = length v

ringAccess :: Ring a -> Int -> V2 a
ringAccess (Ring v) i = v V.! mod i (length v)

ringClamp :: Ring a -> Int -> Int
ringClamp (Ring v) i = mod i (length v)

ringUnpack :: Ring a -> Vector (V2 a)
ringUnpack (Ring v) = v

ringPack :: Vector (V2 a) -> Ring a
ringPack = Ring

ringMap :: (V2 a -> V2 b) -> Ring a -> Ring b
ringMap fn (Ring v) = Ring (V.map fn v)

ringRayIntersect :: Ring Rational -> (Int, Int) -> (Int,Int) -> Maybe (V2 Rational)
ringRayIntersect p (a,b) (c,d) =
  rayIntersect (ringAccess p a, ringAccess p b) (ringAccess p c, ringAccess p d)


area :: Fractional a => V2 a -> V2 a -> V2 a -> a
area a b c = 1/2 * area2X a b c

area2X :: Fractional a => V2 a -> V2 a -> V2 a -> a
area2X (V2 a1 a2) (V2 b1 b2) (V2 c1 c2) =
  det33 (V3 (V3 a1 a2 1)
            (V3 b1 b2 1)
            (V3 c1 c2 1))

epsilon :: Fractional a => a
epsilon = 1e-9

epsEq :: (Ord a, Fractional a) => a -> a -> Bool
epsEq a b = abs (a-b) < epsilon

-- Left turn.
isLeftTurn :: (Fractional a, Ord a) => V2 a -> V2 a -> V2 a -> Bool
isLeftTurn p1 p2 p3 =
  let d = direction p1 p2 p3 in
  case compare d 0 of
    LT -> True
    EQ -> False -- colnear
    GT -> False

isLeftTurnOrLinear :: (Fractional a, Ord a) => V2 a -> V2 a -> V2 a -> Bool
isLeftTurnOrLinear p1 p2 p3 =
  let d = direction p1 p2 p3 in
  case compare d 0 of
    LT -> True
    EQ -> True -- colnear
    GT -> False

isRightTurn :: (Fractional a, Ord a) => V2 a -> V2 a -> V2 a -> Bool
isRightTurn a b c = not (isLeftTurnOrLinear a b c)

isRightTurnOrLinear :: (Fractional a, Ord a) => V2 a -> V2 a -> V2 a -> Bool
isRightTurnOrLinear a b c = not (isLeftTurn a b c)

direction :: Fractional a => V2 a -> V2 a -> V2 a -> a
direction p1 p2 p3 = crossZ (p3-p1) (p2-p1)

isInside :: (Fractional a, Ord a) => V2 a -> V2 a -> V2 a -> V2 a -> Bool
isInside a b c d =
    s >= 0 && s <= 1 && t >= 0 && t <= 1
  where
    (s, t, _) = barycentricCoords a b c d

barycentricCoords :: Fractional a => V2 a -> V2 a -> V2 a -> V2 a -> (a, a, a)
barycentricCoords (V2 x1 y1) (V2 x2 y2) (V2 x3 y3) (V2 x y) =
    (lam1, lam2, lam3)
  where
    lam1 = ((y2-y3)*(x-x3) + (x3 - x2)*(y-y3)) /
           ((y2-y3)*(x1-x3) + (x3-x2)*(y1-y3))
    lam2 = ((y3-y1)*(x-x3) + (x1-x3)*(y-y3)) /
           ((y2-y3)*(x1-x3) + (x3-x2)*(y1-y3))
    lam3 = 1 - lam1 - lam2


{-# INLINE rayIntersect #-}
rayIntersect :: (Fractional a, Ord a) => (V2 a,V2 a) -> (V2 a,V2 a) -> Maybe (V2 a)
rayIntersect (V2 x1 y1,V2 x2 y2) (V2 x3 y3, V2 x4 y4)
  | yBot == 0 = Nothing
  | otherwise = Just $
    V2 (xTop/xBot) (yTop/yBot)
  where
    xTop = (x1*y2 - y1*x2)*(x3-x4) - (x1 - x2)*(x3*y4-y3*x4)
    xBot = (x1-x2)*(y3-y4)-(y1-y2)*(x3-x4)
    yTop = (x1*y2 - y1*x2)*(y3-y4) - (y1-y2)*(x3*y4-y3*x4)
    yBot = (x1-x2)*(y3-y4) - (y1-y2)*(x3-x4)

{-# INLINE isBetween #-}
isBetween :: (Ord a, Fractional a) => V2 a -> (V2 a, V2 a) -> Bool
isBetween (V2 x y) (V2 x1 y1, V2 x2 y2) =
  ((y1 > y) /= (y2 > y) || y == y1 || y == y2) && -- y is between y1 and y2
  ((x1 > x) /= (x2 > x) || x == x1 || x == x2)

{-# INLINE lineIntersect #-}
lineIntersect :: (Ord a, Fractional a) => (V2 a, V2 a) -> (V2 a, V2 a) -> Maybe (V2 a)
lineIntersect a b =
  case rayIntersect a b of
    Just u
      | isBetween u a && isBetween u b -> Just u
    _ -> Nothing

-- circleIntersect :: (Ord a, Fractional a) => (V2 a, V2 a) -> (V2 a, V2 a) -> [V2 a]

distSquared :: (Fractional a) => V2 a -> V2 a -> a
distSquared a b = quadrance (a ^-^ b)

approxDist :: (Real a, Fractional a) => V2 a -> V2 a -> a
approxDist a b = realToFrac (sqrt (realToFrac (distSquared a b) :: Double))

distance' :: (Real a, Fractional a) => V2 a -> V2 a -> Double
distance' a b = sqrt (realToFrac (distSquared a b))

-- sum of angles is always pi.
triangleAngles :: V2 Double -> V2 Double -> V2 Double -> (Double, Double, Double)
triangleAngles a b c =
    (findAngle (b-a) (c-a)
    ,findAngle (c-b) (a-b)
    ,findAngle (a-c) (b-c))
  where
    findAngle v1 v2 = abs (atan2 (crossZ v1 v2) (dot v1 v2))
    -- findAngle v1 v2 = acos (dot v1 v2 / (norm v1 * norm v2))
