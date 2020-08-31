{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module      : Reanimate.Math.Common
Copyright   : Written by David Himmelstrup
License     : Unlicense
Maintainer  : lemmih@gmail.com
Stability   : experimental
Portability : POSIX

Low-level primitives related to computational geometry.

-}
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
  , isLeftTurn          -- :: (Num a, Ord a) => V2 a -> V2 a -> V2 a -> Bool
  , isLeftTurnOrLinear  -- :: (Num a, Ord a) => V2 a -> V2 a -> V2 a -> Bool
  , isRightTurn         -- :: (Num a, Ord a) => V2 a -> V2 a -> V2 a -> Bool
  , isRightTurnOrLinear -- :: (Num a, Ord a) => V2 a -> V2 a -> V2 a -> Bool
  , direction           -- :: Num a => V2 a -> V2 a -> V2 a -> a
  , isInside            -- :: (Fractional a, Ord a) => V2 a -> V2 a -> V2 a -> V2 a -> Bool
  , isInsideStrict      -- :: (Fractional a, Ord a) => V2 a -> V2 a -> V2 a -> V2 a -> Bool
  , barycentricCoords   -- :: Fractional a => V2 a -> V2 a -> V2 a -> V2 a -> (a, a, a)
  , rayIntersect        -- :: (Fractional a, Ord a) => (V2 a,V2 a) -> (V2 a,V2 a) -> Maybe (V2 a)
  , isBetween           -- :: (Ord a, Fractional a) => V2 a -> (V2 a, V2 a) -> Bool
  , lineIntersect       -- :: (Ord a, Fractional a) => (V2 a, V2 a) -> (V2 a, V2 a) -> Maybe (V2 a)
  , distSquared         -- :: (Fractional a) => V2 a -> V2 a -> a
  , approxDist          -- :: (Real a, Fractional a) => V2 a -> V2 a -> a
  , distance'           -- :: (Real a, Fractional a) => V2 a -> V2 a -> Double
  , triangleAngles      -- :: V2 Double -> V2 Double -> V2 Double -> (Double, Double, Double)
  , Epsilon(..)
  ) where

import           Data.Vector    (Vector)
import qualified Data.Vector    as V
import           Linear.Matrix  (det33)
import           Linear.Metric
import           Linear.V2
import           Linear.V3
import           Linear.Vector
import           Linear.Epsilon

instance Epsilon Rational where
  nearZero r = r==0

-- | Circular collection of pairs.
newtype Ring a = Ring (Vector (V2 a))

-- | Number of elements in the ring.
ringSize :: Ring a -> Int
ringSize (Ring v) = length v

-- | Safe method for accessing elements in the ring.
ringAccess :: Ring a -> Int -> V2 a
ringAccess (Ring v) i = v V.! mod i (length v)

-- | Clamp index to within the usable range for the ring.
ringClamp :: Ring a -> Int -> Int
ringClamp (Ring v) i = mod i (length v)

-- | Convert ring to a vector.
ringUnpack :: Ring a -> Vector (V2 a)
ringUnpack (Ring v) = v

-- | Convert vector to a ring.
ringPack :: Vector (V2 a) -> Ring a
ringPack = Ring

-- | Map each element of a ring.
ringMap :: (V2 a -> V2 b) -> Ring a -> Ring b
ringMap fn (Ring v) = Ring (V.map fn v)

-- | Compute the intersection of two pairs of nodes in the ring.
ringRayIntersect :: Ring Rational -> (Int, Int) -> (Int,Int) -> Maybe (V2 Rational)
ringRayIntersect p (a,b) (c,d) =
  rayIntersect (ringAccess p a, ringAccess p b) (ringAccess p c, ringAccess p d)

-- | Compute area of triangle.
area :: Fractional a => V2 a -> V2 a -> V2 a -> a
area a b c = 1/2 * area2X a b c

-- | Compute 2x area of triangle. This avoids a division.
area2X :: Fractional a => V2 a -> V2 a -> V2 a -> a
area2X (V2 a1 a2) (V2 b1 b2) (V2 c1 c2) =
  det33 (V3 (V3 a1 a2 1)
            (V3 b1 b2 1)
            (V3 c1 c2 1))

compareEpsZero :: (Ord a, Fractional a, Epsilon a) => a -> Ordering
compareEpsZero val
  | nearZero val  = EQ
  | otherwise     = compare val 0

{-# INLINE isLeftTurn #-}
-- | Return @True@ iff the line from @p1@ to @p2@ makes a left-turn to @p3@.
isLeftTurn :: (Fractional a, Ord a, Epsilon a) => V2 a -> V2 a -> V2 a -> Bool
isLeftTurn p1 p2 p3 =
  case compareEpsZero (direction p1 p2 p3) of
    LT -> True
    EQ -> False -- colinear
    GT -> False

{-# INLINE isLeftTurnOrLinear #-}
-- | Return @True@ iff the line from @p1@ to @p2@ does not make a right-turn to @p3@.
isLeftTurnOrLinear :: (Fractional a, Ord a, Epsilon a) => V2 a -> V2 a -> V2 a -> Bool
isLeftTurnOrLinear p1 p2 p3 =
  case compareEpsZero (direction p1 p2 p3) of
    LT -> True
    EQ -> True -- colinear
    GT -> False

{-# INLINE isRightTurn #-}
-- | Return @True@ iff the line from @p1@ to @p2@ makes a right-turn to @p3@.
isRightTurn :: (Fractional a, Ord a, Epsilon a) => V2 a -> V2 a -> V2 a -> Bool
isRightTurn a b c = not (isLeftTurnOrLinear a b c)

{-# INLINE isRightTurnOrLinear #-}
-- | Return @True@ iff the line from @p1@ to @p2@ does not make a left-turn to @p3@.
isRightTurnOrLinear :: (Fractional a, Ord a, Epsilon a) => V2 a -> V2 a -> V2 a -> Bool
isRightTurnOrLinear a b c = not (isLeftTurn a b c)

{-# INLINE direction #-}
-- | Compute the change in direction in a line between the three points.
direction :: Num a => V2 a -> V2 a -> V2 a -> a
direction p1 p2 p3 = crossZ (p3-p1) (p2-p1)

{-# INLINE isInside #-}
-- | Returns @True@ if the fourth argument is inside the triangle or
--   on the border.
isInside :: (Fractional a, Ord a) => V2 a -> V2 a -> V2 a -> V2 a -> Bool
isInside a b c d =
    s >= 0 && s <= 1 && t >= 0 && t <= 1 && i >= 0 && i <= 1
  where
    (s, t, i) = barycentricCoords a b c d

{-# INLINE isInsideStrict #-}
-- | Returns @True@ iff the fourth argument is inside the triangle.
isInsideStrict :: (Fractional a, Ord a) => V2 a -> V2 a -> V2 a -> V2 a -> Bool
isInsideStrict a b c d =
    s > 0 && s < 1 && t > 0 && t < 1 && i > 0 && i < 1
  where
    (s, t, i) = barycentricCoords a b c d

{-# INLINE barycentricCoords #-}
-- | Compute relative coordinates inside the triangle. Invariant: @a+b+c=1@
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
-- | Compute intersection of two infinite lines.
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
-- | Returns @True@ iff a point is on a line segment.
isBetween :: (Ord a, Fractional a) => V2 a -> (V2 a, V2 a) -> Bool
isBetween (V2 x y) (V2 x1 y1, V2 x2 y2) =
  ((y1 > y) /= (y2 > y) || y == y1 || y == y2) && -- y is between y1 and y2
  ((x1 > x) /= (x2 > x) || x == x1 || x == x2)

{-# INLINE lineIntersect #-}
-- | Compute intersection of two line segments.
lineIntersect :: (Ord a, Fractional a) => (V2 a, V2 a) -> (V2 a, V2 a) -> Maybe (V2 a)
lineIntersect a b =
  case rayIntersect a b of
    Just u
      | isBetween u a && isBetween u b -> Just u
    _ -> Nothing

-- circleIntersect :: (Ord a, Fractional a) => (V2 a, V2 a) -> (V2 a, V2 a) -> [V2 a]

-- | Compute the square of the distance between two points.
distSquared :: (Num a) => V2 a -> V2 a -> a
distSquared a b = quadrance (a ^-^ b)

-- | Approximate the distance between two points.
approxDist :: (Real a, Fractional a) => V2 a -> V2 a -> a
approxDist a b = realToFrac (sqrt (realToFrac (distSquared a b) :: Double))

-- | Approximate the distance between two points.
distance' :: (Real a, Fractional a) => V2 a -> V2 a -> Double
distance' a b = sqrt (realToFrac (distSquared a b))

-- sum of angles is always pi.
-- | Approximate the angles of a triangle.
triangleAngles :: V2 Double -> V2 Double -> V2 Double -> (Double, Double, Double)
triangleAngles a b c =
    (findAngle (b-a) (c-a)
    ,findAngle (c-b) (a-b)
    ,findAngle (a-c) (b-c))
  where
    findAngle v1 v2 = abs (atan2 (crossZ v1 v2) (dot v1 v2))
    -- findAngle v1 v2 = acos (dot v1 v2 / (norm v1 * norm v2))
