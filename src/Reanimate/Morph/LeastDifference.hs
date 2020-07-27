{-# LANGUAGE OverloadedStrings #-}
module Reanimate.Morph.LeastDifference where

import           Control.Applicative
import           Control.Monad
import           Data.List
import           Data.Ord
import           Data.Maybe
import           Linear.V2
import qualified Data.Vector as V
import           Reanimate.Math.Common
import qualified Reanimate.Math.Compatible as Compat
import           Reanimate.Math.Polygon
import           Reanimate.Morph.Common
import           Reanimate.Morph.Linear

import Debug.Trace

leastDifference :: PointCorrespondence
leastDifference = undefined

pCutsTop :: (Real a, Fractional a) => [(V2 a, V2 a)] -> APolygon a -> [(APolygon a, APolygon a, [Topology], [Topology], (V2 a, V2 a))]
pCutsTop edges p =
  [ (l, r, topL, topR, edge)
  | (l, r) <- pCuts p
  , let edge = (pAccess l 0, pAccess r 1)
        topL = pTopology (edge : edges) l
        topR = pTopology (edge : edges) r
  ]

{-# INLINE pCompatibleCut #-}
pCompatibleCut :: (Real a, Fractional a) => [(Edge a,Edge a)] -> APolygon a -> APolygon a -> Maybe ((APolygon a, APolygon a),(APolygon a, APolygon a), [(Edge a,Edge a)])
pCompatibleCut edges p1 p2 = listToMaybe $ sortOn pairSizeDifference $ do
  (p1l, p1r, aTopL, aTopR, aEdge) <- pCutsTop (map fst edges) p1
  -- let aEdge = (pAccess p1l 0, pAccess p1r 1)
  --     aTopL = pTopology (aEdge : map fst edges) p1l
  --     aTopR = pTopology (aEdge : map fst edges) p1r
  (p2l, p2r, bTopL, bTopR, bEdge) <- pCutsTop (map snd edges) p2
  -- let bEdge = (pAccess p2r 0, pAccess p2r 1)
  --     bTopL = pTopology (bEdge : map snd edges) p2l
  --     bTopR = pTopology (bEdge : map snd edges) p2r
  let newEdges = (aEdge,bEdge) : edges
  -- guard $ pTopology (map fst newEdges) p1l == pTopology (map snd newEdges) p2l
  -- guard $ pTopology (map fst newEdges) p1r == pTopology (map snd newEdges) p2r
  guard $ aTopL == bTopL
  guard $ aTopR == bTopR
  return ((p1l, p1r), (p2l, p2r), newEdges)

sizeDifference :: (Fractional a) => (APolygon a, APolygon a) -> a
sizeDifference (a,b) = abs (pArea a - pArea b)

pairSizeDifference :: (Fractional a, Ord a) => ((APolygon a, APolygon a),(APolygon a, APolygon a),b) -> a
pairSizeDifference (a,b,_) = max (sizeDifference a) (sizeDifference b)

pEdges :: APolygon a -> [Edge a]
pEdges p = [ (pAccess p i, pAccess p $ i+1) | i <- [0 .. pSize p-1]]

type Edge a = (V2 a, V2 a)

data Topology = MutableSegment | ImmutableSegment Int
  deriving (Eq, Show)

{- INLINE pTopology -}
pTopology :: Eq a => [Edge a] -> APolygon a -> [Topology]
pTopology immutableEdges p = worker
  [ maybe MutableSegment ImmutableSegment isImmutable
  | i <- [0 .. pSize p-1]
  , let isImmutable =
          ((pAccess p i, pAccess p $ i+1) `elemIndex` immutableEdges) <|>
          ((pAccess p $ i+1, pAccess p i) `elemIndex` immutableEdges)
  ]
  where
    worker (MutableSegment:MutableSegment:xs) = worker (MutableSegment:xs)
    worker (x:xs)                             = x : worker xs
    worker []                                 = []

triangulate :: Polygon -> Polygon -> [(Polygon,Polygon)]
triangulate a b =
  [ (castPolygon l, castPolygon r)
  | (l,r) <- triangulate_ (castPolygon a) (castPolygon b)
  ]

triangulate_ :: FPolygon -> FPolygon -> [(FPolygon,FPolygon)]
triangulate_ a b
  | distSquared (pCentroid al) (pCentroid bl) +
    distSquared (pCentroid ar) (pCentroid br) <
    distSquared (pCentroid al) (pCentroid br) +
    distSquared (pCentroid ar) (pCentroid bl)
  -- = [(al,bl), (ar,br)]
  = triangulate' edges al bl ++ triangulate' edges ar br
  | otherwise
  -- = [(al,br), (ar,bl)]
  = triangulate' edges al br ++ triangulate' edges ar bl
  where
    edges = [(aEdge,bEdge)]
    aEdge = (pAccess ar 0, pAccess ar 1)
    bEdge = (pAccess br 0, pAccess br 1)
    (al, ar) = pCutEqual a
    (bl, br) = pCutEqual b

{-# SPECIALIZE triangulate' :: [(Edge Double,Edge Double)] -> FPolygon -> FPolygon -> [(FPolygon,FPolygon)] #-}
triangulate' :: (Real a, Fractional a) => [(Edge a,Edge a)] -> APolygon a -> APolygon a -> [(APolygon a,APolygon a)]
triangulate' edges a b
  | pSize a == 3 || pSize b == 3 = giveUp
  -- | trace (show (pSize a, pSize b)) False = undefined
  | Just ((al,ar),(bl,br), newEdges) <- pCompatibleCut edges a b
    = -- traceSVG (helper "topology") $
      triangulate' newEdges al bl ++ triangulate' newEdges ar br
    -- = [(al,bl), (ar,br)]
  | otherwise = giveUp
  where
    giveUp =
      let aNewEdges = max 0 (pSize b - pSize a)
          bNewEdges = max 0 (pSize a - pSize b)
          a' = pAddPointsRestricted (map fst edges) aNewEdges a
          b' = pAddPointsRestricted (map snd edges) bNewEdges b
          -- (a'', b'') = closestLinearCorrespondenceA a' b'
      in [(a, b)]
        --Compat.compatiblyTriangulateP a'' b''

polygonLengths :: Eq a => [APolygon a] -> APolygon a -> [(Int, Int, Int)]
polygonLengths polys origin' = worker 0
  where
    Just firstIdx = V.elemIndex (pAccess (head polys) 0) (polygonPoints origin')
    origin = pSetOffset origin' firstIdx
    polyLength a b
      | V.elem (pAccess a 1) (polygonPoints b)
        = 1 + polyLength (pAdjustOffset a 1) b
      | otherwise
        = 0
    findPoly idx a b = fromMaybe (error $ "Missing: " ++ show idx) $ listToMaybe
      [ (n, p, (i+polygonOffset p) `mod` pSize p)
      | (n, p) <- zip [0..] polys
      , let pts = polygonPoints p
      , Just i <- [V.elemIndex a pts]
      , V.elem b pts
      ]
    worker n | n == pSize origin = []
    worker n =
      let (polyIdx, poly, polyKey) = findPoly n (pAccess origin n) (pAccess origin $ n+1)
          originHere = pAdjustOffset origin n
          len = polyLength originHere poly
      in (polyIdx, polyKey, len) : worker (n+len)


alignPolygons :: (Fractional a, Real a, Eq a) =>
  [(APolygon a, APolygon a)] -> APolygon a -> APolygon a -> [(APolygon a, APolygon a)]
alignPolygons polys originL originR =
    zip
      (worker 0 (map fst polys) diffsL)
      (worker 0 (map snd polys) diffsR)
  where
    lensL = polygonLengths (map fst polys) originL
    lensR = polygonLengths (map snd polys) originR
    diffsL = sortOn sortKey $ zipWith mkDiff lensL lensR
    diffsR = sortOn sortKey $ zipWith mkDiff lensR lensL
    worker n (y:ys) ((i,k,l,d):xs) | n < i
      = y : worker (n+1) ys ((i,k,l,d):xs)
    worker n (y:ys) ((i,k,l,0):xs)
      = worker n (y:ys) xs
    worker n (y:ys) ((i,k,l,d):xs)
      = let y' = pAddPointsBetween (k,l) d y
        in worker n (y':ys) xs
    worker _ ys [] = ys
    worker _ _ _ = error "bad worker input"
    mkDiff (li, lk, ll) (_ri, _rk, rl) =
      (li, lk, ll, max 0 (rl-ll))
    sortKey (i, k, _l, _d) = (i,Down k)

compatTriagPairs :: (Real a, Fractional a) => [(APolygon a, APolygon a)] -> [(APolygon a, APolygon a)]
compatTriagPairs = concatMap (uncurry Compat.compatiblyTriangulateP)

circumference :: (Real a, Fractional a) => [APolygon a] -> APolygon a -> APolygon a
circumference polys origin' = mkPolygon $ V.fromList $ worker 0
  where
    Just firstIdx = V.elemIndex (pAccess (head polys) 0) (polygonPoints origin')
    origin = pSetOffset origin' firstIdx
    fromTo i j p
      | i < j     = [i .. j-1]
      | otherwise = [i .. pSize p-1] ++ [0..j-1]
    check [] = error "no results"
    check [x] = x
    check _ = error "multiple results"
    findPoints n a b = check -- fromMaybe (error $ "Missing: " ++ show n) $ listToMaybe
      [ map (pAccess p) (fromTo i j p)
      | p <- map (flip pSetOffset 0) polys
      , let pts = polygonPoints p
      , Just i <- [V.elemIndex a pts]
      , Just j <- [V.elemIndex b pts]
      , if n == 2 then trace ("Found: " ++ show (i,j)) True else True
      ]
    worker n | n == pSize origin = []
    worker n =
      let a = pAccess origin n
          b = pAccess origin (n+1)
      in findPoints n a b ++ worker (n+1)
