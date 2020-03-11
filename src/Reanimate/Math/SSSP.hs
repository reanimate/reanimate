module Reanimate.Math.SSSP where

import Reanimate.Math.Common
import Reanimate.Math.EarClip
import qualified Data.Vector as V
import Data.Maybe
import Data.Tuple
import qualified Data.Map as Map
import Linear.Metric
import Linear.V2
import Data.List
import Debug.Trace

type SSSP = V.Vector Int

visibleFrom :: Int -> Polygon -> [Int]
visibleFrom y p =
  [ i
  | i <- [0..]
  , i /= y
  , if i < y
    then isLeftTurnOrLinear (p V.! i) (p V.! ((y-1) `mod` n)) (p V.! y) || i+1==y || i-1==y
    else isLeftTurnOrLinear (p V.! y) (p V.! ((y+1) `mod` n)) (p V.! i) || i+1==y || i-1==y
  , let myEdges = [(e1,e2) | (e1,e2) <- edges, e1/=y, e1/=i, e2/=y,e2/=i]
  , all (isNothing . lineIntersect (elt,p V.! i))
          [ (p V.! e1,p V.! e2) | (e1,e2) <- myEdges ]]
  where
    n = length p
    elt = p V.! y
    edges = zip [0..n-1] (tail [0..n-1] ++ [0])

-- O(n^3 log n)
naive :: Polygon -> SSSP
naive p =
    V.fromList $ Map.elems $
    Map.map (fromMaybe 0 . listToMaybe) $
    worker initial [1.. V.length p-1]
  where
    initial = Map.fromList
      [ (v,[])
      | v <- 0:visibleFrom 0 p]
    worker :: Map.Map Int [Int] -> [(Int)] -> Map.Map Int [Int]
    worker m [] = m
    worker m (i:xs) =
      let vs :: [Int]
          vs = visibleFrom i p
          m' = Map.fromList
            [ (v, i : (m Map.! i))
            | v <- vs ]
      in worker (Map.unionWith f m m') xs
    pathLength [] = 0
    pathLength [v] = distance (p V.! v) (p V.! 0)
    pathLength (x:y:xs) = distance (p V.! x) (p V.! y) + pathLength (y:xs)
    f a b =
        case compare (pathLength a) (pathLength b) of
          LT -> a
          _  -> b

type Triangle = (P,P,P)
-- Dual of triangulated polygon
data Dual = Dual (Int,Int,Int) -- (a,b,c)
                  DualTree -- borders ca
                  DualTree -- borders bc
  deriving (Show)

data DualTree
  = EmptyDual
  | NodeLeaf Int
  | NodeDual Int -- axb triangle, a and b are from parent.
      DualTree -- borders ba
      DualTree -- borders xb
  | NodeDualL Int DualTree
  | NodeDualR Int DualTree
  deriving (Show)

-- Dual path:
-- (Int,Int,Int) + V.Vector Int + V.Vector LeftOrRight

simplifyDual :: DualTree -> DualTree
simplifyDual (NodeDual x EmptyDual EmptyDual) = NodeLeaf x
simplifyDual (NodeDual x l EmptyDual) = NodeDualL x l
simplifyDual (NodeDual x EmptyDual r) = NodeDualR x r
simplifyDual dual = dual

dual :: Triangulation -> Dual
dual t =
  case t V.! 0 of
    [] -> Dual (0,1,V.length t-1) EmptyDual (dualTree t (1, (V.length t-1)) 0)
    (x:_) -> Dual (0,1,x) (dualTree t (x,0) 1) (dualTree t (1,x) 0)

dualTree :: Triangulation -> (Int,Int) -> Int -> DualTree
dualTree t (a,b) e = -- simplifyDual $
    case hasTriangle of
      [] -> EmptyDual
      [(ab)] ->
        NodeDual ab (dualTree t (ab,b) a) (dualTree t (a,ab) b)
      _ -> error "Invalid triangulation"
  where
    hasTriangle = nub $ (findTriangles a b) ++ (findTriangles b a)
    n = V.length t
    next x = (x+1) `mod` n
    prev x = (x-1) `mod` n
    -- Find diagonals of 'f'
    -- that are next to 'g' (+1 or -1, mod n)
    findTriangles f g
      | next f == prev g && next f /= e = [next f]
      | next g == prev f && next g /= e = [next g]
    findTriangles f g =
      [ v | v <- t V.! f
      , v /= e , v == next g || v == prev g ]


--toDualTree (Dual (a,b,c) l r) = NodeDual c l r
-- O(n)
-- sssp :: Dual -> SSSP
sssp (Dual (a,b,c) l r) =
    worker [c] [b] a r ++
    loopLeft c l
  where
    loopLeft outer l =
      case l of
        EmptyDual -> []
        NodeDual x l' r' ->
          worker [x] [outer] a r' ++
          loopLeft x l'
    worker _ _ _ EmptyDual = []
    worker f1 f2 cusp (NodeDual x l r)
      | x is visible from cusp =
        (x, cusp) :
        worker f1 [x] cusp l ++
        worker [x] f cusp r
      | (f1Hi, v, f1Lo) <- v is along f1 =
        (x, v) :
        worker f1Hi [x] v l ++
        worker [x] fiLo cusp r
      | (f2Hi, v, f2Lo) <- v is along f2 =
        (x, v) :
        worker f2Hi [x] cusp l ++
        worker [x] f2Lo v

{-
(7,0)
(1,0)
funnel: 7,0  0  0,1
2 is neither to the left of the left funnel or to the right of the right funnel,
therefore it is visible from 0.
(2,0)

-}
