{-# LANGUAGE RecordWildCards #-}
module Reanimate.Math.Common where

import           Control.Applicative
import           Data.List           (tails)
import           Data.List.Split
import           Data.Vector         (Vector)
import qualified Data.Vector         as V
import           Linear.Matrix
import           Linear.Metric
import           Linear.V2
import           Linear.V3
import           Linear.Vector
import           Text.Printf

type Polygon = V.Vector P
type P = V2 Double

type Triangulation = V.Vector [Int]


triangle :: [P]
triangle = [V2 1 1, V2 0 0, V2 2 0]

triangle' :: [P]
triangle' = reverse [V2 1 1, V2 0 0, V2 2 0]

shape1 :: [P]
shape1 =
  [ V2 0 0, V2 2 0
  , V2 2 1, V2 2 2, V2 2 3, V2 2 4, V2 2 5, V2 2 6
  , V2 1 1, V2 0 1 ]

shape2 :: [P]
shape2 =
  [ V2 0 0, V2 1 0, V2 1 1, V2 2 1, V2 2 (-1), V2 0 (-1), V2 0 (-2)
  , V2 3 (-2), V2 3 2, V2 0 2]

shape3 :: [P]
shape3 =
  [ V2 0 0, V2 1 0, V2 1 1, V2 2 1, V2 2 2, V2 0 2]

shape4 :: [P]
shape4 =
  [ V2 0 0, V2 1 0, V2 1 1, V2 2 1, V2 2 (-1), V2 3 (-1),V2 3 2, V2 0 2]

shape5 :: [P]
shape5 = cyclePolygons shape4 !! 2

-- square
shape6 :: [P]
shape6 = [ V2 0 0, V2 1 0, V2 1 1, V2 0 1 ]

concave :: [P]
concave = [V2 0 0, V2 2 0, V2 2 2, V2 1 1, V2 0 2]

cyclePolygons :: [P] -> [[P]]
cyclePolygons p = take (length p) $ map (take (length p)) $ tails (cycle p)

interpFirst (x:y:xs) t =
  lerp t y x : y : xs ++ [x]

area :: P -> P -> P -> Double
area a b c = 1/2 * area2X a b c

area2X :: P -> P -> P -> Double
area2X (V2 a1 a2) (V2 b1 b2) (V2 c1 c2) =
  det33 (V3 (V3 a1 a2 1)
            (V3 b1 b2 1)
            (V3 c1 c2 1))

isConvex :: [P] -> Bool
isConvex vertices = and
  [ area2X (vertices!!i) (vertices!!j) (vertices!!k) > 0
  | i <- [0..n-1]
  , j <- [i+1..n-1]
  , k <- [j+1..n-1]
  ]
  where n = length vertices

isCCW :: [P] -> Bool
isCCW [] = False
isCCW (x:xs) = sum (zipWith fn (x:xs) (xs++[x])) < 0
  where
    fn (V2 x1 y1) (V2 x2 y2) = (x2-x1)*(y2+y1)

-- O(n)
-- Returns true if ac can be cut from polygon. That is, true if 'b' is an ear.
-- isEarCorner polygon a b c = True iff ac can be cut
isEarCorner :: [P] -> P -> P -> P -> Bool
isEarCorner polygon a b c =
    isLeftTurn a b c && -- If it is a right turn then the line ac will be outside the polygon
    and [ not (isInside a b c k)
    | k <- polygon, k /= a && k /= b && k /= c
    ]

epsilon :: Fractional a => a
epsilon = 1e-9

epsEq :: (Ord a, Fractional a) => a -> a -> Bool
epsEq a b = abs (a-b) < epsilon

-- Left turn.
isLeftTurn :: P -> P -> P -> Bool
isLeftTurn p1 p2 p3 =
  let d = direction p1 p2 p3 in
  if abs d < epsilon
    then False -- colinear
    else d < 0

isLeftTurnOrLinear :: P -> P -> P -> Bool
isLeftTurnOrLinear p1 p2 p3 =
  let d = direction p1 p2 p3 in
  if abs d < epsilon
    then True -- colinear
    else d < 0

isRightTurn :: P -> P -> P -> Bool
isRightTurn a b c = not (isLeftTurn a b c)

direction :: P -> P -> P -> Double
direction p1 p2 p3 = crossZ (p3-p1) (p2-p1)

isInside :: P -> P -> P -> P -> Bool
isInside a b c d =
    s >= 0 && s <= 1 && t >= 0 && t <= 1
  where
    (s, t, _) = barycentricCoords a b c d

barycentricCoords :: P -> P -> P -> P -> (Double, Double, Double)
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

distSquared :: P -> P -> Double
distSquared a b = quadrance (a ^-^ b)
