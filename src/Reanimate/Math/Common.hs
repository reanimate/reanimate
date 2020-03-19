{-# LANGUAGE RecordWildCards #-}
module Reanimate.Math.Common where

import           Data.List     (intersect, tails)
import           Data.Vector   (Vector)
import qualified Data.Vector   as V
import           Linear.Matrix (det33)
import           Linear.Metric
import           Linear.V2
import           Linear.V3
import           Linear.Vector
-- import Debug.Trace

-- Generate random polygons, options:
--   1. put corners around a circle. Vary the radius.
--   2. close a hilbert curve
type FPolygon = Vector P
-- Optimize representation?
--   Polygon = (Vector XNumerator, Vector XDenominator
--             ,Vector YNumerator, Vector YDenominator)
type Polygon = Vector (V2 Rational)
type P = V2 Double

-- When is a polygon valid/simple?
--   It is counter-clockwise.
--   No edges intersect.
-- O(n^2)
isSimple :: Polygon -> Bool
isSimple p | length p < 3 = False
isSimple p = isCCW p && checkEdge 0 2
  where
    len = length p
    -- check i,i+1 against j,j+1
    -- j > i+1
    checkEdge i j
      | j >= len = if i < len-4 then checkEdge (i+1) (i+3) else True
      | otherwise =
        case lineIntersect (pAccess p i, pAccess p $ i+1)
                           (pAccess p j, pAccess p $ pNext p j) of
          Just u | u /= pAccess p i -> False
          _nothing                  -> checkEdge i (j+1)

-- isSimple :: Polygon -> Bool
-- isSimple p = isCCW p &&
--     and [ checkEdge i | i <- [0 .. length p-1]]
--   where
--     checkEdge i = and
--       [ isNothing $
--         lineIntersect
--           (pAccess p i, pAccess p $ pNext p i)
--           (pAccess p j, pAccess p $ pNext p j)
--       | j <- [0 .. length p-1], j /= i, j /= pNext p i, j /= pPrev p i ]

-- Place n points on a circle, use one parameter to slide the points back and forth.
-- Use second parameter to move points closer to center circle.
genPolygon :: Double -> [(Double, Double)] -> Polygon
genPolygon radius points
  | len < 4 = error "genPolygon: require at least three points"
  | otherwise = V.fromList
  [ V2 (realToFrac $ cos ang * pointRadius)
       (realToFrac $ sin ang * pointRadius)
  | (i,(angMod,rMod))  <- zip [0..] points
  , let minAngle = tau / len * i - pi
        maxAngle = tau / len * (i+1) - pi
        ang = minAngle + (maxAngle-minAngle)*angMod
        pointRadius = rMod * radius
  ]
  where
    tau = 2*pi
    len = fromIntegral (length points)

unpackPolygon :: Polygon -> [(Double, Double)]
unpackPolygon p =
    [ worker i (fmap realToFrac e)
    | (i,e) <- zip [0..] (V.toList p) ]
  where
    len = fromIntegral (length p)
    worker i (V2 x y) =
      let ang = atan2 y x
          minAngle = tau / len * i - pi
          maxAngle = tau / len * (i+1) - pi
      in ((ang-minAngle)/(maxAngle-minAngle), sqrt (x*x+y*y))
    tau = 2*pi

-- Max edges: n-2
-- Each edge is represented twice: 2n-4
-- Flat structure:
--   edges   :: V.Vector Int -- max length (2n-4)
--   offsets :: V.Vector Int -- length n
-- Combine the two vectors? < n => offsets, >= n => edges?
type Triangulation = V.Vector [Int]

-- When is a triangulation valid?
--   Intersection: No internal edges intersect.
--   Completeness: All edge neighbours share an internal edge.
-- i, i+1
isValidTriangulation :: Polygon -> Triangulation -> Bool
isValidTriangulation p t = isComplete && intersectionFree
  where
    isComplete = all isProper [0 .. length p-1]
    isProper i =
      let j = pNext p i in
      length ((pPrev p i : (t V.! i)) `intersect` (pNext p j : t V.! j)) == 1
    intersectionFree = and
      [ case (lineIntersect (pAccess p a, pAccess p b) (pAccess p c, pAccess p d)) of
          Nothing -> True
          Just u  -> u == pAccess p a || u == pAccess p b ||
                     u == pAccess p c || u == pAccess p d
      | ((a,b),(c,d)) <- edgePairs ]
    edgePairs = [ (e1, e2) | (e1, rest) <- zip edges (drop 1 $ tails edges), e2 <- rest]
    edges =
      [ (n, i)
      | (n, lst) <- zip [0..] (V.toList t)
      , i <- lst
      , n < i
      ]

pMod :: Vector (V2 a) -> Int -> Int
pMod p i = i `mod` V.length p

pNext :: Vector (V2 a) -> Int -> Int
pNext p i = pMod p (i+1)

pPrev :: Vector (V2 a) -> Int -> Int
pPrev p i = pMod p (i-1)

pAccess :: Vector (V2 a) -> Int -> V2 a
pAccess p i = p V.! i -- V.unsafeIndex p i

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

cyclePolygon :: (Real a, Fractional a, Ord a) => Vector (V2 a) -> Double -> Vector (V2 a)
cyclePolygon p 0 = p
cyclePolygon p t = worker 0 0
  where
    worker acc i
      -- | segment + acc == limit =
      --   V.drop i p <>
      --   V.take i p
      | segment + acc > limit =
        V.singleton (lerp (realToFrac $ (segment + acc - limit)/segment) x y) <>
        V.drop (i+1) p <>
        V.take (i+1) p
      | i == V.length p-1  = p
      | otherwise = worker (acc+segment) (i+1)
        where
          x = pAccess p i
          y = pAccess p $ pNext p i
          segment = sqrt (realToFrac (distSquared x y))
    len = polygonLength' p
    limit = t * len

polygonLength :: (Real a, Fractional a) => Vector (V2 a) -> a
polygonLength p = sum
  [ realToFrac (sqrt (realToFrac (distSquared (pAccess p i) (pAccess p $ pNext p i)) :: Double))
  | i <- [0 .. V.length p-1]]

polygonLength' :: (Real a, Fractional a) => Vector (V2 a) -> Double
polygonLength' p = sum
  [ sqrt (realToFrac (distSquared (pAccess p i) (pAccess p $ pNext p i)) :: Double)
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
  ((y1 > y) /= (y2 > y)) && -- y is between y1 and y2
  ((x1 > x) /= (x2 > x))

{-# INLINE lineIntersect #-}
lineIntersect :: (Ord a, Fractional a) => (V2 a, V2 a) -> (V2 a, V2 a) -> Maybe (V2 a)
lineIntersect a b =
  case rayIntersect a b of
    Just u
      | isBetween u a && isBetween u b -> Just u
    _ -> Nothing

distSquared :: (Fractional a) => V2 a -> V2 a -> a
distSquared a b = quadrance (a ^-^ b)
