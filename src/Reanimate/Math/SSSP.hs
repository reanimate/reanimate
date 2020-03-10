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

visibleFrom :: Int -> [P] -> [Int]
visibleFrom y p =
  [ (i)
  | (i,z) <- zip [0..] p
  , i /= y
  , if i < y
    then isLeftTurnOrLinear (p!!i) (p!!((y-1) `mod` n)) (p!!y) || i+1==y || i-1==y
    else isLeftTurnOrLinear (p!!y) (p!!((y+1) `mod` n)) (p!!i) || i+1==y || i-1==y
  , let myEdges = [(e1,e2) | (e1,e2) <- edges, e1/=y, e1/=i, e2/=y,e2/=i]
  , let b = all (isNothing . lineIntersect (elt,z))
          [ (p!!e1,p!!e2) | (e1,e2) <- myEdges ]
  , b ]
  where
    n = length p
    elt = p!!y
    edges = zip [0..n-1] (tail [0..n-1] ++ [0])

-- O(n^3 log n)
naive :: [P] -> V.Vector Int
naive p@(x:xs) =
    V.fromList $ Map.elems $
    Map.map (fromMaybe 0 . listToMaybe) $
    worker initial [1.. length p-1]
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
    pathLength [v] = distance (p!!v) x
    pathLength (x:y:xs) = distance (p!!x) (p!!y) + pathLength (y:xs)
    f a b =
        case compare (pathLength a) (pathLength b) of
          LT -> a
          _  -> b

type Triangle = (P,P,P)
-- Dual of triangulated polygon
data Dual = Dual (Int,Int,Int) DualTree DualTree
  deriving (Show)
data DualTree
  = EmptyDual
  | NodeDual Int DualTree DualTree
  deriving (Show)

dual :: Triangulation -> Dual
dual t =
  case t V.! 0 of
    [] -> Dual (0,1,V.length t-1) (dualTree t (1, (V.length t-1)) 0) EmptyDual
    (x:_) -> Dual (0,1,x) (dualTree t (1,x) 0) (dualTree t (x,0) 1)

dualTree :: Triangulation -> (Int,Int) -> Int -> DualTree
dualTree t (a,b) e =
    case hasTriangle of
      Nothing -> EmptyDual
      Just (x,y,z) ->
        let [ab] = [x,y,z] \\ [a,b]
        in NodeDual ab (dualTree t (a,ab) b) (dualTree t (ab,b) a)
  where
    hasTriangle = listToMaybe (intersect (findTriangles a) (findTriangles b))
    n = V.length t
    findTriangles x =
      [ if (x+1)`mod` n == (v-1)`mod`n
          then (x,(x+1)`mod`n,v)
          else (v,(v+1)`mod`n,x)
      | v <- t V.! x
      , v /= e
      ]
