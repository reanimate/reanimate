{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Reanimate.Math.SSSP where

import           Control.Monad
import           Control.Monad.ST
import           Data.FingerTree         (SearchResult (..), (|>))
import qualified Data.FingerTree         as F
import           Data.List
import qualified Data.Map                as Map
import           Data.Maybe
import           Data.STRef
import           Data.Tree
import qualified Data.Vector             as V
import qualified Data.Vector.Mutable     as MV
-- import           Debug.Trace
import           Reanimate.Math.Common
import           Reanimate.Math.EarClip

type SSSP = V.Vector Int

visibleFrom :: Int -> Polygon -> [Int]
visibleFrom y p =
  [ i
  | i <- [0.. n-1]
  , i /= y
  , let pY = pAccess p y
        pYn = pAccess p $ pNext p y
        pYp = pAccess p $ pPrev p y
        pI = pAccess p i
        isOpen = isRightTurn pYp pY pYn
  , pNext p y == i || pPrev p y == i || if isOpen
    then isLeftTurnOrLinear pY pYn pI ||
         isLeftTurnOrLinear pYp pY pI
    else not $ isRightTurn pY pYn pI ||
               isRightTurn pYp pY pI
  , let myEdges = [(e1,e2) | (e1,e2) <- edges, e1/=y, e1/=i, e2/=y,e2/=i]
  , all (isNothing . lineIntersect (elt,p V.! i))
          [ (p V.! e1,p V.! e2) | (e1,e2) <- myEdges ]]
  where
    n = length p
    elt = pAccess p y
    edges = zip [0..n-1] (tail [0..n-1] ++ [0])



-- Iterative Single Source Shortest Path solver. Quite slow.
naive :: Polygon -> SSSP
naive p =
    V.fromList $ Map.elems $
    Map.map snd $
    worker initial
  where
    initial = Map.singleton 0 (0,0)
    visibilityArray =
      V.fromList
      [ visibleFrom i p | i <- [0..length p-1]]
    worker :: Map.Map Int (Rational, Int) -> Map.Map Int (Rational, Int)
    worker m
        | m==newM   = newM
        | otherwise = worker newM
      where
        ms' = [ Map.fromList
                    [ case Map.lookup v m of
                        Nothing -> (v, (distThroughI, i))
                        Just (otherDist,parent)
                          | otherDist > distThroughI -> (v, (distThroughI, i))
                          | otherwise -> (v, (otherDist, parent))
                    | v <- visibilityArray V.! i
                    , let distThroughI = dist + approxDist (pAccess p i) (pAccess p v) ]
              | (i,(dist,_)) <- Map.toList m
              ]
        newM = Map.unionsWith g (m:ms') :: Map.Map Int (Rational,Int)
    g a b = if fst a < fst b then a else b

naive2 :: Polygon -> SSSP
naive2 p = runST $ do
    parents <- MV.replicate (length p) (-1)
    costs <- MV.replicate (length p) (-1)
    MV.write parents 0 0
    MV.write costs 0 0
    changedRef <- newSTRef False
    let loop i
          | i == length p = do
            changed <- readSTRef changedRef
            when changed $ do
              writeSTRef changedRef False
              loop 0
          | otherwise = do
            myCost <- MV.read costs i
            unless (myCost < 0) $
              forM_ (visibilityArray V.! i) $ \n -> do
                -- n is visible from i.
                theirCost <- MV.read costs n
                let throughCost = myCost + approxDist (pAccess p i) (pAccess p n)
                when (throughCost < theirCost || theirCost < 0) $ do
                    MV.write parents n i
                    MV.write costs n throughCost
                    writeSTRef changedRef True
            loop (i+1)
    loop 0
    V.unsafeFreeze parents
  where
    visibilityArray =
      V.fromList
      [ visibleFrom i p | i <- [0..length p-1]]

-- Dual of triangulated polygon
data Dual = Dual (Int,Int,Int) -- (a,b,c)
                  DualTree -- borders ca
                  DualTree -- borders bc
  deriving (Show)

data DualTree
  = EmptyDual
  | NodeDual Int -- axb triangle, a and b are from parent.
      DualTree -- borders xb
      DualTree -- borders ax
  deriving (Show)

drawDual :: Dual -> String
drawDual d = drawTree $
  case d of
    Dual (a,b,c) l r -> Node (show (a,b,c)) [worker c a l, worker b c r]
  where
    worker _a _b EmptyDual = Node "Leaf" []
    worker a b (NodeDual x l r) =
      Node (show (b,a,x)) [worker x b l, worker a x r]

dualToTriangulation :: Polygon -> Dual -> Triangulation
dualToTriangulation p d = edgesToTriangulation p $ filter goodEdge $
    case d of
      -- 0,1,5
      -- 1,5
      --
      Dual (a,b,c) l r ->
        (a,b):(a,c):(b,c):worker c a l ++ worker b c r
  where
    goodEdge (a,b)
      = a /= pNext p b && a /= pPrev p b
    worker _a _b EmptyDual = []
    worker a b (NodeDual x l r) =
      (a,x) : (x, b) : worker x b l ++ worker a x r

-- Dual path:
-- (Int,Int,Int) + V.Vector Int + V.Vector LeftOrRight

simplifyDual :: DualTree -> DualTree
-- simplifyDual (NodeDual x EmptyDual EmptyDual) = NodeLeaf x
-- simplifyDual (NodeDual x l EmptyDual) = NodeDualL x l
-- simplifyDual (NodeDual x EmptyDual r) = NodeDualR x r
simplifyDual d = d

dual :: Triangulation -> Dual
dual t =
  case hasTriangle of
    []    -> error "weird triangulation"
    -- [] -> Dual (0,1,V.length t-1) EmptyDual (dualTree t (1, (V.length t-1)) 0)
    (x:_) -> Dual (0,1,x) (dualTree t (x,0) 1) (dualTree t (1,x) 0)
  where
    hasTriangle = (n-1 : t V.! 0) `intersect` (2 : t V.! 1)
    n = V.length t

-- a=6, b=0, e=1
dualTree :: Triangulation -> (Int,Int) -> Int -> DualTree
dualTree t (a,b) e = -- simplifyDual $
    case hasTriangle of
      [] -> EmptyDual
      [(ab)] ->
        NodeDual ab
          (dualTree t (ab,b) a)
          (dualTree t (a,ab) b)
      _ -> error $ "Invalid triangulation: " ++ show (a,b,e,hasTriangle)
  where
    hasTriangle = (prev a : next a : t V.! a) `intersect` (prev b : next b : t V.! b)
      \\ [e]
    n = V.length t
    next x = (x+1) `mod` n
    prev x = (x-1) `mod` n

data MinMax = MinMax Int Int | MinMaxEmpty deriving (Show)
instance Semigroup MinMax where
  MinMaxEmpty <> b = b
  a <> MinMaxEmpty = a
  MinMax a b <> MinMax c d = MinMax (min a c) (max b d)
instance Monoid MinMax where
  mempty = MinMaxEmpty

instance F.Measured MinMax Int where
  measure i = MinMax i i

-- O(n)
sssp :: Polygon -> Dual -> SSSP
sssp p d = toSSSP $
    case d of
      Dual (a,b,c) l r ->
        (a, a) :
        (b, a) :
        (c, a) :
        worker (F.singleton c) (F.singleton b) a r ++
        loopLeft a c l
  where
    toSSSP edges =
      (V.fromList . map snd . sortOn fst) edges
    loopLeft a outer l =
      case l of
        EmptyDual -> []
        NodeDual x l' r' ->
          (x,a) :
          worker (F.singleton x) (F.singleton outer) a r' ++
          loopLeft a x l'
    searchFn _cusp _x MinMaxEmpty _ = False
    searchFn _cusp _x _ MinMaxEmpty = True
    searchFn _cusp x (MinMax a _) (MinMax _ b) =
      isRightTurn (p V.! a) (p V.! b) (p V.! x)
    searchFn2 _cusp _x MinMaxEmpty _ = False
    searchFn2 _cusp _x _ MinMaxEmpty = True
    searchFn2 _cusp x (MinMax _ a) (MinMax b _) =
      isLeftTurn (p V.! a) (p V.! b) (p V.! x)
    isOnLeft f cusp x =
      case F.viewl f of
        F.EmptyL -> False
        v F.:< _ -> isLeftTurnOrLinear (pAccess p cusp) (pAccess p v) (pAccess p x)
    isOnRight f cusp x =
      case F.viewl f of
        F.EmptyL -> False
        v F.:< _ -> isRightTurnOrLinear (pAccess p cusp) (pAccess p v) (pAccess p x)
    worker _ _ _ EmptyDual = []
    worker f1 f2 cusp (NodeDual x l r) =
        --trace ("Funnel: " ++ show (f1,cusp,f2,x)) $
        if isOnLeft f1 cusp x
          then -- trace ("Search: " ++ show (F.search (searchFn cusp x) f1)) $
            case F.search (searchFn cusp x) f1 of
              Position f1Lo v f1Hi ->
                -- trace ("Visble from left: " ++ show (x,v)) $
                (x, v::Int) :
                worker f1Hi (F.singleton x) v l ++
                worker (f1Lo |> v |> x) f2 cusp r
              _ -> error "invalid sssp"
          else if isOnRight f2 cusp x
            then -- trace ("Search: " ++ show (F.search (searchFn2 cusp x) f2)) $
              case F.search (searchFn2 cusp x) f2 of
                Position f2Lo v f2Hi ->
                  (x, v::Int) :
                  worker f1 (f2Lo |> v |> x) cusp l ++
                  worker (F.singleton x) f2Hi v r
                _ -> error "invalid sssp"
            else
              -- trace ("Visble from cusp: " ++ show (x,cusp)) $
              (x, cusp::Int) :
              worker f1 (F.singleton x) cusp l ++
              worker (F.singleton x) f2 cusp r
{-
(7,0)
(1,0)
funnel: 7,0  0  0,1
2 is neither to the left of the left funnel or to the right of the right funnel,
therefore it is visible from 0.
(2,0)

-}
