{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Reanimate.Math.SSSP where

import           Control.Monad
import           Control.Monad.ST
import           Data.FingerTree        (SearchResult (..), (|>))
import qualified Data.FingerTree        as F
import           Data.List
import qualified Data.Map               as Map
import           Data.Maybe
import           Data.Ord
import           Data.STRef
import           Data.Tree
import qualified Data.Vector            as V
import qualified Data.Vector.Mutable    as MV
import           Linear.V2
-- import           Debug.Trace
import           Reanimate.Math.Triangulate
import           Reanimate.Math.Common
import           Reanimate.Math.EarClip

type SSSP = V.Vector Int


-- ssspParent :: Polygon -> SSSP -> Int -> Int
-- ssspParent p sTree x =
--     (sTree V.! ((x - polygonOffset p) `mod` n) + polygonOffset p) `mod` n
--   where
--     n = polygonSize p

visibilityArray :: Ring Rational -> V.Vector [Int]
visibilityArray p = arr
  where
    n = ringSize p
    arr = V.fromList
        [ visibility y
        | y <- [0..n-1]
        ]
    visibility y =
      [ i
      | i <- [0..y-1]
      , y `elem` arr V.! i ] ++
      [ i
      | i <- [y+1 .. n-1]
      , let pI = ringAccess p i
            isOpen = isRightTurn pYp pY pYn
      , ringClamp p (y+1) == i || ringClamp p (y-1) == i || if isOpen
        then isLeftTurnOrLinear pY pYn pI ||
             isLeftTurnOrLinear pYp pY pI
        else not $ isRightTurn pY pYn pI ||
                   isRightTurn pYp pY pI
      , let myEdges = [(e1,e2) | (e1,e2) <- edges, e1/=y, e1/=i, e2/=y,e2/=i]
      , all (isNothing . lineIntersect (pY,pI))
              [ (ringAccess p e1, ringAccess p e2) | (e1,e2) <- myEdges ]]
      where
        pY = ringAccess p y
        pYn = ringAccess p $ y+1
        pYp = ringAccess p $ y-1
        edges = zip [0..n-1] (tail [0..n-1] ++ [0])



-- Iterative Single Source Shortest Path solver. Quite slow.
naive :: Ring Rational -> SSSP
naive p =
    V.fromList $ Map.elems $
    Map.map snd $
    worker initial
  where
    initial = Map.singleton 0 (0,0)
    visibility = visibilityArray p
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
                    | v <- visibility V.! i
                    , let distThroughI = dist + approxDist (ringAccess p i) (ringAccess p v) ]
              | (i,(dist,_)) <- Map.toList m
              ]
        newM = Map.unionsWith g (m:ms') :: Map.Map Int (Rational,Int)
    g a b = if fst a < fst b then a else b

naive2 :: Ring Rational -> SSSP
naive2 p = runST $ do
    parents <- MV.replicate (ringSize p) (-1)
    costs <- MV.replicate (ringSize p) (-1)
    MV.write parents 0 0
    MV.write costs 0 0
    changedRef <- newSTRef False
    let loop i
          | i == ringSize p = do
            changed <- readSTRef changedRef
            when changed $ do
              writeSTRef changedRef False
              loop 0
          | otherwise = do
            myCost <- MV.read costs i
            unless (myCost < 0) $
              forM_ (visibility V.! i) $ \n -> do
                -- n is visible from i.
                theirCost <- MV.read costs n
                let throughCost = myCost + approxDist (ringAccess p i) (ringAccess p n)
                when (throughCost < theirCost || theirCost < 0) $ do
                    MV.write parents n i
                    MV.write costs n throughCost
                    writeSTRef changedRef True
            loop (i+1)
    loop 0
    V.unsafeFreeze parents
  where
    visibility = visibilityArray p

data PDual = PDual (V.Vector Int) Rational [PDual]
  deriving (Show)

toPDual :: Ring Rational -> Dual -> PDual
toPDual p d =
  case d of
    Dual (a,b,c) l r ->
      PDual (V.fromList [a,b,c])
        (area2X (ringAccess p a) (ringAccess p b) (ringAccess p c))
        (catMaybes [ worker c a l, worker b c r])
  where
    worker _ _ EmptyDual = Nothing
    worker a b (NodeDual x l r) = Just $
      PDual (V.fromList [a,x,b])
        (area2X (ringAccess p a) (ringAccess p x) (ringAccess p b))
        (catMaybes [ worker x b l, worker a x r])

pdualSize :: PDual -> Int
pdualSize (PDual _ _ children) = 1 + sum (map pdualSize children)

pdualArea :: PDual -> Rational
pdualArea (PDual _ faceArea _) = faceArea

-- FIXME: 'origin' isn't used. Remove.
pdualReduce :: Ring Rational -> PDual -> Int -> PDual
pdualReduce origin pdual n
  | pdualSize pdual <= n = pdual
  | otherwise =
    let smallest = minimum $ pAreas pdual
    in pdualReduce origin (merge smallest pdual) n
  where
    merge _s (PDual p faceArea []) = PDual p faceArea []
    merge s (PDual p faceArea children)
      | faceArea == s =
        let (PDual p2 area2 children2:xs) = sortBy (comparing pdualArea) children
        in PDual (joinP p p2) (faceArea+area2) (children2++xs)
      | otherwise =
        let (PDual p2 area2 children2:xs) = sortBy (comparing pdualArea) children
        in if area2 == s
            then PDual (joinP p p2) (faceArea+area2) (children2++xs)
            else PDual p faceArea (map (merge s) children)
    pAreas (PDual _ faceArea children) = faceArea : concatMap pAreas children
    joinP a b = V.fromList (sort (V.toList a ++ V.toList b))

pdualRings :: Ring Rational -> PDual -> [Ring Rational]
pdualRings p (PDual pts _area children) =
  ringPack (V.map (ringAccess p) pts) : concatMap (pdualRings p) children

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

dualToTriangulation :: Ring Rational -> Dual -> Triangulation
dualToTriangulation p d = edgesToTriangulation (ringSize p) $ filter goodEdge $
    case d of
      Dual (a,b,c) l r ->
        (a,b):(a,c):(b,c):worker c a l ++ worker b c r
  where
    goodEdge (a,b)
      = a /= ringClamp p (b+1) && a /= ringClamp p (b-1)
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

dual :: Int -> Triangulation -> Dual
dual root t =
  case hasTriangle of
    []    -> error "weird triangulation"
    -- [] -> Dual (0,1,V.length t-1) EmptyDual (dualTree t (1, (V.length t-1)) 0)
    (x:_) -> Dual (root,rootNext,x) (dualTree t (x,root) rootNext) (dualTree t (rootNext,x) root)
  where
    rootNext = idx (root+1)
    rootPrev = idx (root-1)
    rootNNext = idx (root+2)
    idx i = i `mod` n
    hasTriangle = (rootPrev : t V.! root) `intersect` (rootNNext : t V.! rootNext)
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
sssp :: (Fractional a, Ord a) => Ring a -> Dual -> SSSP
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
      isRightTurn (ringAccess p a) (ringAccess p b) (ringAccess p x)
    searchFn2 _cusp _x MinMaxEmpty _ = False
    searchFn2 _cusp _x _ MinMaxEmpty = True
    searchFn2 _cusp x (MinMax _ a) (MinMax b _) =
      isLeftTurn (ringAccess p a) (ringAccess p b) (ringAccess p x)
    isOnLeft f cusp x =
      case F.viewl f of
        F.EmptyL -> False
        v F.:< _ -> isLeftTurnOrLinear (ringAccess p cusp) (ringAccess p v) (ringAccess p x)
    isOnRight f cusp x =
      case F.viewl f of
        F.EmptyL -> False
        v F.:< _ -> isRightTurnOrLinear (ringAccess p cusp) (ringAccess p v) (ringAccess p x)
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
