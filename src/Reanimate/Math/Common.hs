{-# LANGUAGE RecordWildCards #-}
module Reanimate.Math.Common where

import           Control.Applicative
import           Data.List.Split
import           Data.Vector         (Vector)
import qualified Data.Vector         as V
import           Linear.Matrix
import           Linear.V2
import           Linear.V3
import           Text.Printf

type P = V2 Double

triangle :: [P]
triangle = [V2 1 1, V2 0 0, V2 2 0]

triangle' :: [P]
triangle' = reverse [V2 1 1, V2 0 0, V2 2 0]

concave :: [P]
concave = [V2 0 0, V2 2 0, V2 2 2, V2 1 1, V2 0 2]

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

epsilon :: Double
epsilon = 1e-9

-- Left turn or straight.
isLeftTurn :: P -> P -> P -> Bool
isLeftTurn p1 p2 p3 =
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
