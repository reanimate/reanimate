{-# LANGUAGE RecordWildCards #-}
module Reanimate.Math.Common where

import           Data.Vector         (Vector)
import qualified Data.Vector         as V
import           Linear.Matrix       (det33)
import           Linear.Metric
import           Linear.V2
import           Linear.V3
import           Linear.Vector

-- Generate random polygons, options:
--   1. put corners around a circle. Vary the radius.
--   2. close a hilbert curve
type Polygon = Vector P
type RPolygon = Vector (V2 Rational)
type P = V2 Double

-- Max edges: n-2
-- Each edge is represented twice: 2n-4
-- Flat structure:
--   edges   :: V.Vector Int -- max length (2n-4)
--   offsets :: V.Vector Int -- length n
-- Combine the two vectors? < n => offsets, >= n => edges?
type Triangulation = V.Vector [Int]

pMod :: Polygon -> Int -> Int
pMod p i = i `mod` V.length p

pNext :: Polygon -> Int -> Int
pNext p i = pMod p (i+1)

pPrev :: Polygon -> Int -> Int
pPrev p i = pMod p (i-1)

pAccess :: Polygon -> Int -> P
pAccess p i = p V.! i

triangle :: Polygon
triangle = V.fromList [V2 1 1, V2 0 0, V2 2 0]

triangle' :: [P]
triangle' = reverse [V2 1 1, V2 0 0, V2 2 0]

shape1 :: Polygon
shape1 = V.fromList
  [ V2 0 0, V2 2 0
  , V2 2 1, V2 2 2, V2 2 3, V2 2 4, V2 2 5, V2 2 6
  , V2 1 1, V2 0 1 ]

shape2 :: Polygon
shape2 = V.fromList
  [ V2 0 0, V2 1 0, V2 1 1, V2 2 1, V2 2 (-1), V2 0 (-1), V2 0 (-2)
  , V2 3 (-2), V2 3 2, V2 0 2]

shape3 :: Polygon
shape3 = V.fromList
  [ V2 0 0, V2 1 0, V2 1 1, V2 2 1, V2 2 2, V2 0 2]

shape4 :: Polygon
shape4 = V.fromList
  [ V2 0 0, V2 1 0, V2 1 1, V2 2 1, V2 2 (-1), V2 3 (-1),V2 3 2, V2 0 2]

shape5 :: Polygon
shape5 = cyclePolygons shape4 !! 2

-- square
shape6 :: Polygon
shape6 = V.fromList [ V2 0 0, V2 1 0, V2 1 1, V2 0 1 ]

concave :: Polygon
concave = V.fromList [V2 0 0, V2 2 0, V2 2 2, V2 1 1, V2 0 2]

cyclePolygons :: Polygon -> [Polygon]
cyclePolygons p =
  [ V.drop n p <> V.take n p
  | n <- [0 .. len-1]]
  where
    len = V.length p

cyclePolygon :: Polygon -> Double -> Polygon
cyclePolygon p 0 = p
cyclePolygon p t = worker 0 0
  where
    worker acc i
      -- | segment + acc == limit =
      --   V.drop i p <>
      --   V.take i p
      | segment + acc > limit =
        V.singleton (lerp ((segment + acc - limit)/segment) x y) <>
        V.drop (i+1) p <>
        V.take (i+1) p
      | i == V.length p-1  = p
      | otherwise = worker (acc+segment) (i+1)
        where
          x = pAccess p i
          y = pAccess p $ pNext p i
          segment = distance x y
    len = polygonLength p
    limit = t * len

polygonLength :: Polygon -> Double
polygonLength p = sum
  [ distance (pAccess p i) (pAccess p $ pNext p i)
  | i <- [0 .. V.length p-1]]

area :: Fractional a => V2 a -> V2 a -> V2 a -> a
area a b c = 1/2 * area2X a b c

area2X :: Fractional a => V2 a -> V2 a -> V2 a -> a
area2X (V2 a1 a2) (V2 b1 b2) (V2 c1 c2) =
  det33 (V3 (V3 a1 a2 1)
            (V3 b1 b2 1)
            (V3 c1 c2 1))

isConvex :: Polygon -> Bool
isConvex p = and
  [ area2X (pAccess p i) (pAccess p j) (pAccess p k) > 0
  | i <- [0..n-1]
  , j <- [i+1..n-1]
  , k <- [j+1..n-1]
  ]
  where n = V.length p

isCCW :: Polygon -> Bool
isCCW p | V.null p = False
isCCW p =
    (V.sum (V.zipWith fn p (V.drop 1 p)) + fn (V.last p) (V.head p)) < 0
  where
    fn (V2 x1 y1) (V2 x2 y2) = (x2-x1)*(y2+y1)

-- O(n)
-- Returns true if ac can be cut from polygon. That is, true if 'b' is an ear.
-- isEarCorner polygon a b c = True iff ac can be cut
isEarCorner :: Polygon -> [Int] -> Int -> Int -> Int -> Bool
isEarCorner p polygon a b c =
    isLeftTurn (pAccess p a) (pAccess p b) (pAccess p c) &&
    -- If it is a right turn then the line ac will be outside the polygon
    and [ not (isInside (pAccess p a) (pAccess p b) (pAccess p c) (pAccess p k))
    | k <- polygon, k /= a && k /= b && k /= c
    ]

epsilon :: Fractional a => a
epsilon = 1e-9

epsEq :: (Ord a, Fractional a) => a -> a -> Bool
epsEq a b = abs (a-b) < epsilon

-- Left turn.
isLeftTurn :: (Fractional a, Ord a) => V2 a -> V2 a -> V2 a -> Bool
isLeftTurn p1 p2 p3 =
  let d = direction p1 p2 p3 in
  if abs d < epsilon
    then False -- colinear
    else d < 0

isLeftTurnOrLinear :: (Fractional a, Ord a) => V2 a -> V2 a -> V2 a -> Bool
isLeftTurnOrLinear p1 p2 p3 =
  let d = direction p1 p2 p3 in
  if abs d < epsilon
    then True -- colinear
    else d < 0

isRightTurn :: (Fractional a, Ord a) => V2 a -> V2 a -> V2 a -> Bool
isRightTurn a b c = not (isLeftTurnOrLinear a b c)

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


rayIntersect :: Fractional a => (V2 a,V2 a) -> (V2 a,V2 a) -> V2 a
rayIntersect (V2 x1 y1,V2 x2 y2) (V2 x3 y3, V2 x4 y4) =
    V2 (xTop/xBot) (yTop/yBot)
  where
    xTop = (x1*y2 - y1*x2)*(x3-x4) - (x1 - x2)*(x3*y4-y3*x4)
    xBot = (x1-x2)*(y3-y4)-(y1-y2)*(x3-x4)
    yTop = (x1*y2 - y1*x2)*(y3-y4) - (y1-y2)*(x3*y4-y3*x4)
    yBot = (x1-x2)*(y3-y4) - (y1-y2)*(x3-x4)

isBetween :: (Ord a, Fractional a) => V2 a -> (V2 a, V2 a) -> Bool
isBetween (V2 x y) (V2 x1 y1, V2 x2 y2) =
  ((y1 > y) /= (y2 > y) || epsEq y y1||epsEq y y2) && -- y is between y1 and y2
  ((x1 > x) /= (x2 > x) || epsEq x x1||epsEq x x2)

lineIntersect :: (Ord a, Fractional a) => (V2 a, V2 a) -> (V2 a, V2 a) -> Maybe (V2 a)
lineIntersect a b
  | isBetween u a && isBetween u b = Just u
  | otherwise     = Nothing
  where u = rayIntersect a b

distSquared :: (Fractional a) => V2 a -> V2 a -> a
distSquared a b = quadrance (a ^-^ b)
