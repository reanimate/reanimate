{-# LANGUAGE RecordWildCards #-}
module Reanimate.Math.Polygon
  ( module Reanimate.Math.Polygon
  , module Reanimate.Math.Common
  ) where

import           Data.Hashable
import           Data.List                  (intersect, tails, sort, maximumBy)
import           Data.Maybe
import           Data.Ratio
import           Data.Serialize
import           Data.Vector                (Vector)
import qualified Data.Vector                as V
import           Linear.V2
import           Linear.Vector
import           Reanimate.Math.Common
import           Reanimate.Math.EarClip
import           Reanimate.Math.SSSP
import           Reanimate.Math.Triangulate

-- import Debug.Trace

-- Generate random polygons, options:
--   1. put corners around a circle. Vary the radius.
--   2. close a hilbert curve
type FPolygon = APolygon Double
-- Optimize representation?
--   Polygon = (Vector XNumerator, Vector XDenominator
--             ,Vector YNumerator, Vector YDenominator)
data APolygon a = Polygon
  { polygonPoints        :: Vector (V2 a)
  , polygonOffset        :: Int
  , polygonTriangulation :: Triangulation
  , polygonSSSP          :: Vector SSSP
  }
type Polygon = APolygon Rational
type P = V2 Double

instance Show a => Show (APolygon a) where
  show = show . V.toList . polygonPoints

instance Hashable a => Hashable (APolygon a) where
  hashWithSalt s p = V.foldl' hashWithSalt s (polygonPoints p)

instance (Fractional a, Ord a, Serialize a) => Serialize (APolygon a) where
  put = put . V.toList . polygonPoints
  get = (mkPolygon . V.fromList) <$> get

polygonRing :: APolygon a -> Ring a
polygonRing = ringPack . polygonPoints

mkPolygon :: (Fractional a, Ord a) => V.Vector (V2 a) -> APolygon a
mkPolygon points = Polygon
    { polygonPoints = points
    , polygonOffset = 0
    , polygonTriangulation = trig
    , polygonSSSP = V.generate n $ \i -> sssp ring (dual i trig)
    }
  where
    n = length points
    ring = ringPack points
    trig = earClip ring

mkPolygonFromRing :: (Fractional a, Ord a) => Ring a -> APolygon a
mkPolygonFromRing = mkPolygon . ringUnpack

polygonUnsafeMap :: (Ring a -> Ring a) -> APolygon a -> APolygon a
polygonUnsafeMap = undefined

-- polygonParent p i j = shortest-path parent from j to i
polygonParent :: Polygon -> Int -> Int -> Int
polygonParent p i j =
    (sTree V.! (mod (j + polygonOffset p) n) - polygonOffset p) `mod` n
  where
    sTree = polygonSSSP p V.! (mod (i + polygonOffset p) n)
    n = polygonSize p

polygonCopy :: Polygon -> Polygon
polygonCopy p = mkPolygon $ V.generate (polygonSize p) $ pAccess p

setOffset :: APolygon a -> Int -> APolygon a
setOffset p offset =
  p { polygonOffset = offset `mod` polygonSize p }

modOffset :: APolygon a -> Int -> APolygon a
modOffset p offset =
  p { polygonOffset = (polygonOffset p + offset) `mod` polygonSize p }

polygonSize :: APolygon a -> Int
polygonSize = length . polygonPoints

polygonNull :: APolygon a -> Bool
polygonNull = V.null . polygonPoints

pNext :: APolygon a -> Int -> Int
pNext p i = (i+1) `mod` polygonSize p

pPrev :: APolygon a -> Int -> Int
pPrev p i = (i-1) `mod` polygonSize p

-- When is a polygon valid/simple?
--   It is counter-clockwise.
--   No edges intersect.
-- O(n^2)
-- 'checkEdge' takes 90% of the time.
isSimple :: Polygon -> Bool
isSimple p | polygonSize p < 3 = False
isSimple p = isCCW p && noDups && checkEdge 0 2
  where
    noDups = checkForDups (sort (V.toList (polygonPoints p)))
    checkForDups (x:y:xs)
      = x /= y && checkForDups (y:xs)
    checkForDups _ = True
    len = polygonSize p
    -- check i,i+1 against j,j+1
    -- j > i+1
    checkEdge i j
      | j >= len = if i < len-4 then checkEdge (i+1) (i+3) else True
      | otherwise =
        case lineIntersect (pAccess p i, pAccess p $ i+1)
                           (pAccess p j, pAccess p $ j+1) of
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

scalePolygon :: Rational -> Polygon -> Polygon
scalePolygon s = mkPolygon . V.map (\v -> v ^* s) . polygonPoints

centerPolygon :: Polygon -> Polygon
centerPolygon p = mkPolygon $ V.map (subtract c) $ polygonPoints p
  where c = polygonCentroid p ^/ 2

translatePolygon :: V2 Rational -> Polygon -> Polygon
translatePolygon v = mkPolygon . V.map (+v) . polygonPoints

-- Returns (min-x, min-y, width, height)
polygonBox :: Polygon -> (Rational, Rational, Rational, Rational)
polygonBox = \p ->
    let V2 x y = pAccess p 0 in
    case V.foldl' worker (x, y, 0, 0) (polygonPoints p) of
      (xMin, yMin, xMax, yMax) ->
        (xMin, yMin, xMax-xMin, yMax-yMin)
  where
    worker (xMin,yMin,xMax,yMax) (V2 thisX thisY) =
      (min xMin thisX, min yMin thisY
      ,max xMax thisX, max yMax thisY)

-- Place n points on a circle, use one parameter to slide the points back and forth.
-- Use second parameter to move points closer to center circle.
genPolygon :: [(Double, Double)] -> Polygon
genPolygon points
  | len < 4 = error "genPolygon: require at least four points"
  | otherwise = mkPolygon $ V.fromList
  [ V2 (realToFrac $ cos ang * rMod)
       (realToFrac $ sin ang * rMod)
  | (i,(angMod,rMod))  <- zip [0..] points
  , let minAngle = tau / len * i - pi
        maxAngle = tau / len * (i+1) - pi
        ang = minAngle + (maxAngle-minAngle)*angMod
  ]
  where
    tau = 2*pi
    len = fromIntegral (length points)

unpackPolygon :: Polygon -> [(Double, Double)]
unpackPolygon p =
    [ worker i (fmap realToFrac e)
    | (i,e) <- zip [0..] (V.toList $ polygonPoints p) ]
  where
    len = fromIntegral (polygonSize p)
    worker i (V2 x y) =
      let ang = atan2 y x
          minAngle = tau / len * i - pi
          maxAngle = tau / len * (i+1) - pi
      in ((ang-minAngle)/(maxAngle-minAngle), sqrt (x*x+y*y))
    tau = 2*pi

-- When is a triangulation valid?
--   Intersection: No internal edges intersect.
--   Completeness: All edge neighbours share a single internal edge.
isValidTriangulation :: Polygon -> Triangulation -> Bool
isValidTriangulation p t = isComplete && intersectionFree
  where
    o = polygonOffset p
    isComplete = all isProper [0 .. polygonSize p-1]
    isProper i =
      let j = pNext p i in
      length ((pPrev p i : (t V.! i)) `intersect` (pNext p j : t V.! j)) == 1
    intersectionFree = and
      [ case (lineIntersect (pAccess p (a-o), pAccess p (b-o)) (pAccess p (c-o), pAccess p (d-o))) of
          Nothing -> True
          Just u  -> u == pAccess p (a-o) || u == pAccess p (b-o) ||
                     u == pAccess p (c-o) || u == pAccess p (d-o)
      | ((a,b),(c,d)) <- edgePairs ]
    edgePairs = [ (e1, e2) | (e1, rest) <- zip edges (drop 1 $ tails edges), e2 <- rest]
    edges =
      [ (n, i)
      | (n, lst) <- zip [0..] (V.toList t)
      , i <- lst
      , n < i
      ]

triangulationsToPolygons :: Polygon -> Triangulation -> [Polygon]
triangulationsToPolygons p t =
  [ mkPolygon $ V.fromList
    [ pAccess p g, pAccess p i, pAccess p j ]
  | i <- [0 .. polygonSize p-1]
  , let js = filter (i<) $ t V.! i
  , (g, j) <- zip (i-1:js) js
  ]

polygonIsInside :: Polygon -> V2 Rational -> Bool
polygonIsInside p point = or
  [ isInside (rawAccess g) (rawAccess i) (rawAccess j) point
  | i <- [0 .. polygonSize p-1]
  , let js = filter (i<) $ (polygonTriangulation p) V.! i
  , (g, j) <- zip (i-1:js) js
  ]
  where
    rawAccess x = polygonPoints p V.! x

-- reducePolygons :: Int -> [Polygon] -> [Polygon]
-- reducePolygons n ps
--   | length ps <= n = ps
--   | otherwise =
--     let p = findSmallest ps
--         es = edges p
--         e = findSmallest es
--     in reducePolygons n (merge p e : delete p (delete e ps))
--   where
--     findSmallest = minimumBy (comparing area2X)
--     shareEdge p1 p2 =


pAccess :: APolygon a -> Int -> V2 a
pAccess p i = polygonPoints p V.! ((polygonOffset p + i) `mod` polygonSize p)

triangle :: Polygon
triangle = mkPolygon $ V.fromList [V2 1 1, V2 0 0, V2 2 0]

triangle' :: [P]
triangle' = reverse [V2 1 1, V2 0 0, V2 2 0]

shape1 :: Polygon
shape1 = mkPolygon $ V.fromList
  [ V2 0 0, V2 2 0
  , V2 2 1, V2 2 2, V2 2 3, V2 2 4, V2 2 5, V2 2 6
  , V2 1 1, V2 0 1 ]

shape2 :: Polygon
shape2 = mkPolygon $ V.fromList
  [ V2 0 0, V2 1 0, V2 1 1, V2 2 1, V2 2 (-1), V2 0 (-1), V2 0 (-2)
  , V2 3 (-2), V2 3 2, V2 0 2]

shape3 :: Polygon
shape3 = mkPolygon $ V.fromList
  [ V2 0 0, V2 1 0, V2 1 1, V2 2 1, V2 2 2, V2 0 2]

shape4 :: Polygon
shape4 = mkPolygon $ V.fromList
  [ V2 0 0, V2 1 0, V2 1 1, V2 2 1, V2 2 (-1), V2 3 (-1),V2 3 2, V2 0 2]

shape5 :: Polygon
shape5 = cyclePolygons shape4 !! 2

-- square
shape6 :: Polygon
shape6 = mkPolygon $ V.fromList [ V2 0 0, V2 1 0, V2 1 1, V2 0 1 ]

shape7 :: Polygon
shape7 = scalePolygon 6 $ mkPolygon $ V.fromList
        [V2 ((-1567171105775771) % 144115188075855872) ((-7758063241391039) % 1152921504606846976)
        ,V2 ((-2711114907999263) % 18014398509481984) ((-3561889280168807) % 18014398509481984)
        ,V2 ((-6897139157863177) % 72057594037927936) ((-1632144794297397) % 4503599627370496)
        ,V2 (5592137945106423 % 36028797018963968) ((-71351641856107) % 281474976710656)
        ,V2 (2568147525079071 % 4503599627370496) ((-4312925637247687) % 18014398509481984)
        ,V2 (1291079014395023 % 2251799813685248) (321513444515769 % 2251799813685248)
        ,V2 (2071709221627247 % 4503599627370496) (4019115966736491 % 9007199254740992)
        ,V2 ((-1589087869859839) % 144115188075855872) (4904023654354179 % 9007199254740992)
        ,V2 ((-2328090886101149) % 36028797018963968) (2587887893460759 % 36028797018963968)
        ,V2 ((-7990199074159871) % 18014398509481984) (1301850651537745 % 4503599627370496)]

shape8 :: Polygon
shape8 = scalePolygon 10 $ genPolygon
          [(0.36,0.4),(0.7,1.8e-2),(0.7,0.2),(0.1,0.4),(0.2,0.2),(0.7,0.1),(0.4,8.0e-2)]

shape9 :: Polygon
shape9 = scalePolygon 5 $ genPolygon
  [(0.5,0.2),(0.7,0.6),(0.4,0.3),(0.1,0.7),(0.3,1.0e-2),(0.5,0.3),(0.2,0.8),(0.1,0.8),(0.7,6.0e-2),(0.1,0.6)]

shape10 :: Polygon
shape10 = genPolygon
  [(0.4,0.7),(0.2,0.2),(0.3,0.9),(5.0e-2,0.1),(0.7,1.0e-2),(0.7,0.9),(0.2,0.1),(0.5,6.0e-2),(0.6,9.0e-2)]

shape11 :: Polygon
shape11 = genPolygon
  [(0.1,0.8),(0.7,0.6),(0.7,0.4),(0.3,0.5),(0.8,0.9),(0.8,6.0e-2),(1.0e-2,4.0e-2),(0.8,0.1)]

shape12 :: Polygon
shape12 = mkPolygon $ V.fromList
  [ V2 0 0, V2 0.5 1.5, V2 2 2, V2 (-2) 2, V2 (-0.5) 1.5 ]

-- F shape
shape13 :: Polygon
shape13 = cyclePolygons (mkPolygon $ V.reverse (V.fromList
  [ V2 0 0, V2 0 2
  , V2 1 2, V2 1 1.7, V2 0.3 1.7, V2 0.3 1
  , V2 1 1, V2 1 0.7
  , V2 0.3 0.7, V2 0.3 0 ])) !! 7

-- E shape
shape14 :: Polygon
shape14 = cyclePolygons (mkPolygon $ V.reverse $ V.fromList
  [ V2 0 0, V2 0 2 -- up
  , V2 1 2, V2 1 1.7, V2 0.3 1.7, V2 0.3 1 -- first prong
  , V2 1 1, V2 1 0.7, V2 0.3 0.7, V2 0.3 0.3 -- second prong
  , V2 1 0.3, V2 1 0 -- last prong
  ]) !! 9

--
shape15 :: Polygon
shape15 = mkPolygon $ V.fromList
  [ V2 0 0, V2 2 0
  , V2 2 2, V2 1 2
  , V2 1 1, V2 0 1]

shape16 :: Polygon
shape16 = mkPolygon $ V.fromList
  [ V2 0 0, V2 2 0
  , V2 2 1, V2 1 1
  , V2 1 2, V2 0 2]

shape17 :: Polygon
shape17 = mkPolygon $ V.fromList
  [ V2 2 0, V2 2 1
  , V2 1 1, V2 1 2
  , V2 0 2, V2 0 1, V2 0 0 ]

shape18 :: Polygon
shape18 = mkPolygon $ V.fromList
  [ V2 2 0, V2 2 1, V2 2 2
  , V2 1 2, V2 1 1
  , V2 0 1, V2 0 0 ]

shape19 :: Polygon
shape19 = mkPolygon $ V.fromList
  [ V2 (-3) (-3), V2 0 (-1)
  , V2 3 (-3), V2 1 0
  , V2 3 3, V2 0 1
  , V2 (-3) 3, V2 (-1) 0 ]

shape20 :: Polygon
shape20 = mkPolygon $ V.fromList
  [ V2 (-3) (-3)
  , V2 0 (-1)
  , V2 3 (-3)
  , V2 5 0
  , V2 2.5 (-2)
  , V2 1 0
  , V2 3 3
  , V2 0 1
  , V2 (-3) 3
  , V2 (-1) 0 ]

shape21 :: Polygon
shape21 = mkPolygon $ V.fromList
  [V2 0.0 0.0,V2 1.0 0.0,V2 1.0 1.0,V2 2.0 1.0,V2 2.0 (-1.0),V2 3.0 (-1.0)
  ,V2 3.0 2.0,V2 0.0 2.0]

shape22 :: Polygon
shape22 = scalePolygon 2 $ mkPolygon $ V.fromList
  [V2 (-0.17) (-0.08)
  ,V2 (-0.34) (-0.21)
  ,V2 0.0 0.0
  ,V2 (-0.10) 0.60
  ,V2 (-0.14) 0.19
  ,V2 (-0.05) 0.03
  ]

concave :: Polygon
concave = mkPolygon $
  V.fromList [V2 0 0, V2 2 0, V2 2 2, V2 1 1, V2 0 2]

winding :: Int -> Polygon
winding n | n < 1 = error "Polygon must have at least one winding."
winding n = mkPolygon $
    V.fromList $ p0 : p1 : walkTo p1 1 n (V2 1 0) ++ reverse (walkTo p0 1 (n+2) (V2 (-1) 0))
  where
    p0 = V2 0 0
    p1 = V2 0 1
    walkTo at a b dir
      | a == b = []
      | otherwise =
        let newAt = at + (dir ^* toRational a)
        in newAt : walkTo newAt (a+1) b (rot dir)
    rot (V2 x y) =
      V2 y (-x)

cyclePolygons :: Polygon -> [Polygon]
cyclePolygons p = map (modOffset p) [0 .. polygonSize p-1]

cyclePolygon :: (Real a, Fractional a, Ord a) => APolygon a -> Double -> APolygon a
cyclePolygon p 0 = p
cyclePolygon p t = mkPolygon $ worker 0 0
  where
    worker acc i
      -- | segment + acc == limit =
      --   V.drop i p <>
      --   V.take i p
      | segment + acc > limit =
        V.singleton (lerp (realToFrac $ (segment + acc - limit)/segment) x y) <>
        -- V.drop (i+1) (polygonPoints p) <>
        V.fromList (map (pAccess p) [i+1..polygonSize p-1]) <>
        V.fromList (map (pAccess p) [0 .. i])
        -- V.take (i+1) (polygonPoints p)
      | i == polygonSize p-1  = V.fromList (map (pAccess p) [0 .. polygonSize p-1])
      | otherwise = worker (acc+segment) (i+1)
        where
          x = pAccess p i
          y = pAccess p $ i+1
          segment = distance' x y
    len = polygonLength' p
    limit = t * len

polygonCentroid :: Polygon -> V2 Rational
polygonCentroid p = V2 cx cy
  where
    pArea = polygonArea p
    cx = recip (6*pArea) * V.sum (mapEdges fnX p)
    cy = recip (6*pArea) * V.sum (mapEdges fnY p)
    fnX (V2 x y) (V2 x' y') = (x+x')*(x*y' - x'*y)
    fnY (V2 x y) (V2 x' y') = (y+y')*(x*y' - x'*y)

mapEdges :: (V2 Rational -> V2 Rational -> a) -> Polygon -> V.Vector a
mapEdges fn p = V.generate (polygonSize p) $ \i ->
  fn (pAccess p i) (pAccess p $ i+1)

polygonArea :: Polygon -> Rational
polygonArea p =
  0.5 * V.sum (mapEdges (\(V2 x y) (V2 x' y') -> x*y' - x'*y) p)

polygonLength :: (Real a, Fractional a) => APolygon a -> a
polygonLength p = sum
  [ approxDist (pAccess p i) (pAccess p $ i+1)
  | i <- [0 .. polygonSize p-1]]

polygonLength' :: (Real a, Fractional a) => APolygon a -> Double
polygonLength' p = sum
  [ distance' (pAccess p i) (pAccess p $ i+1)
  | i <- [0 .. polygonSize p-1]]


-- Add points by splitting the longest lines in half repeatedly.
polygonAddPoints :: Int -> Polygon -> Polygon
polygonAddPoints n p | n <= 0 = p
polygonAddPoints n p = polygonAddPoints (n-1) $
    mkPolygon $ V.fromList $ concatMap worker [0 .. polygonSize p-1]
  where
    worker idx
      | idx == longestEdge =
        let start = pAccess p idx
            end = pAccess p $ idx+1
            middle = lerp 0.5 end start
        in [start, middle]
      | otherwise = [pAccess p idx]
    longestEdge = maximumBy cmpLength [0 .. polygonSize p-1]
    cmpLength a b =
      distSquared (pAccess p a) (pAccess p $ a+1) `compare`
      distSquared (pAccess p b) (pAccess p $ b+1)

-- addPoints :: Int -> Polygon -> Polygon
-- addPoints n p = mkPolygon $ V.fromList $ worker n 0 (map (pAccess p) [0..s])
--   where
--     worker 0 _ rest = init rest
--     worker i acc (x:y:xs) =
--       let xy = approxDist x y in
--       if acc + xy > limit
--         then x : worker (i-1) 0 (lerp ((limit-acc)/xy) y x : y:xs)
--         else x : worker i (acc+xy) (y:xs)
--     worker _ _ [_] = []
--     worker _ _ _ = error "addPoints: invalid polygon"
--     s = polygonSize p
--     len = polygonLength p
--     limit = len / fromIntegral (n+1)

isConvex :: Polygon -> Bool
isConvex p = and
  [ area2X (pAccess p i) (pAccess p j) (pAccess p k) > 0
  | i <- [0..n-1]
  , j <- [i+1..n-1]
  , k <- [j+1..n-1]
  ]
  where n = polygonSize p

isCCW :: Polygon -> Bool
isCCW p | polygonNull p = False
isCCW p = V.sum (mapEdges fn p) < 0
  where
    fn (V2 x1 y1) (V2 x2 y2) = (x2-x1)*(y2+y1)

pRayIntersect :: Polygon -> (Int, Int) -> (Int,Int) -> Maybe (V2 Rational)
pRayIntersect p (a,b) (c,d) =
  rayIntersect (pAccess p a, pAccess p b) (pAccess p c, pAccess p d)





---------------------------------------------------------
-- SSSP visibility and SSSP windows

ssspVisibility :: Polygon -> Polygon
ssspVisibility p = mkPolygon $
    V.fromList $ clearDups $ go [0 .. polygonSize p-1] -- ([root..polygonSize p-1]  ++ [0 .. root-1])
  where
    clearDups (x:y:xs)
      | x == y = clearDups (y:xs)
      | otherwise = x : clearDups (y:xs)
    clearDups xs = xs
    obstructedBy n =
      case polygonParent p 0 n of
        0 -> n
        i -> obstructedBy i
    go [] = []
    go [x] = [pAccess p x]
    go (x:y:xs) =
      let xO = obstructedBy x
          yO = obstructedBy y
      in case () of
          ()
            -- Both ends are visible.
            | xO == x && yO == y -> pAccess p x : go (y:xs)
            -- X is visible, x to intersect (0,yO) (x,y)
            | xO == x   ->
              pAccess p x : fromMaybe (pAccess p y) (pRayIntersect p (0,yO) (x,y)) : go (y:xs)
            -- Y is visible
            | yO == y   -> fromMaybe (pAccess p x) (pRayIntersect p (0,xO) (x,y)) : pAccess p y : go (y:xs)
            -- Neither is visible and they've obstructed by the same point
            -- so the entire edge is hidden.
            | xO == yO -> go (y:xs)
            -- Neither is visible. Cast shadow from obstruction points to
            -- find if a subsection of the edge is visible.
            | otherwise ->
              let a = fromMaybe (error "a") (pRayIntersect p (0,xO) (x,y))
                  b = fromMaybe (error "b") (pRayIntersect p (0,yO) (x,y))
              in if a /= b
                then a : b : go (y:xs)
                else go (y:xs)

ssspWindows :: Polygon -> [(V2 Rational, V2 Rational)]
ssspWindows p = clearDups $ go (pAccess p 0) [0..polygonSize p-1]
  where
    clearDups (x:y:xs)
      | x == y = clearDups (y:xs)
      | otherwise = x : clearDups (y:xs)
    clearDups xs = xs
    obstructedBy n =
      case polygonParent p 0 n of
        0 -> n
        i -> obstructedBy i
    go _ [] = []
    go _ [_] = []
    go l (x:y:xs) =
      let xO = obstructedBy x
          yO = obstructedBy y
      in case () of
          ()
            -- Both ends are visible.
            | xO == x && yO == y -> go (pAccess p x) (y:xs)
            -- X is visible, x to intersect (0,yO) (x,y)
            | xO == x   ->
              go (fromMaybe (pAccess p y) (pRayIntersect p (0,yO) (x,y))) (y:xs)
            -- Y is visible
            | yO == y   ->
              let newL = fromMaybe (pAccess p x) (pRayIntersect p (0,xO) (x,y)) in
              (l, newL) :
              go newL (y:xs)
            -- Neither is visible and they've obstructed by the same point
            -- so the entire edge is hidden.
            | xO == yO -> go l (y:xs)
            -- Neither is visible. Cast shadow from obstruction points to
            -- find if a subsection of the edge is visible.
            | otherwise ->
              let a = fromMaybe (error "a") (pRayIntersect p (0,xO) (x,y))
                  b = fromMaybe (error "b") (pRayIntersect p (0,yO) (x,y))
              in if a /= b
                then (l, a) : (b, pAccess p yO) : go (pAccess p yO) (y:xs)
                else go l (y:xs)

pdualPolygons :: Polygon -> PDual -> [Polygon]
pdualPolygons p pdual = map mkPolygonFromRing (pdualRings (polygonRing p) pdual)
