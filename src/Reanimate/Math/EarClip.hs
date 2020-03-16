module Reanimate.Math.EarClip where


import           Data.List
import           Data.Maybe
import qualified Data.Set              as Set
import qualified Data.Vector           as V
import           Linear.V2

import           Reanimate.Math.Common
import           Reanimate.Math.DCEL

import           Debug.Trace


-- FIXME: Move to Common or a Triangulation module
-- O(n^2), can be improved to O(n)
edgesToTriangulation :: Polygon -> [(Int,Int)] -> Triangulation
edgesToTriangulation p edges = V.fromList
  [ nub $ [ e2 | (e1,e2) <- edges, i == e1 ] ++ [ e1 | (e1,e2) <- edges, i == e2 ]
  | i <- [0.. V.length p-1]
  ]

-- Triangulation by ear clipping. O(n^2)
earClip :: Polygon -> Triangulation
earClip = last . earClip'

earClip' :: Polygon -> [Triangulation]
earClip' p = map (edgesToTriangulation p) $ inits $
  let ears = Set.fromList [ i
             | i <- elts
             , isEarCorner p elts (pPrev p i) i (pNext p i) ]
  in worker ears (mkQueue elts)
  where
    elts = [0 .. V.length p-1]
    pts = V.toList p
    -- worker :: Set.Set Int -> PolyQueue Int -> [(P,P)]
    worker ears queue | isSimple queue = []
    worker ears queue
      -- | trace (show (x, Set.member x ears)) False = undefined
      | x `Set.member` ears =
        let dq = dropQ queue
            v0 = prevQ 1 queue
            v1 = prevQ 0 queue
            v3 = peekQ dq
            v4 = peekQ (nextQ dq)
            e1 = if isEarCorner p (toList dq) v0 v1 v3
                  then Set.insert v1 ears
                  else Set.delete v1 ears
            e2 = if isEarCorner p (toList dq) v1 v3 v4
                  then Set.insert v3 e1
                  else Set.delete v3 e1
        in (v1,v3) : worker e2 dq
      | otherwise = worker ears (nextQ queue)
      where
        x = peekQ queue

data PolyQueue a = PolyQueue [a] [a] [a]

sizeQ :: PolyQueue a -> Int
sizeQ (PolyQueue a b _) = length a + length b

mkQueue :: [a] -> PolyQueue a
mkQueue pts = PolyQueue pts [] (reverse pts)

toList :: PolyQueue a -> [a]
toList (PolyQueue a b _) = a ++ b

isSimple :: PolyQueue a -> Bool
isSimple (PolyQueue xs ys _) =
  case xs ++ ys of
    [_,_,_] -> True
    _       -> False

peekQ :: PolyQueue a -> a
peekQ (PolyQueue (x:_) _ _) = x

nextQ :: PolyQueue a -> PolyQueue a
nextQ (PolyQueue [x] ys p)    = PolyQueue (reverse (x:ys)) [] (x:p)
nextQ (PolyQueue (x:xs) ys p) = PolyQueue xs (x:ys) (x:p)

dropQ :: PolyQueue a -> PolyQueue a
dropQ (PolyQueue [x] ys p)    = PolyQueue (reverse ys) [] p
dropQ (PolyQueue (x:xs) ys p) = PolyQueue xs ys p

prevQ :: Int -> PolyQueue a -> a
prevQ nth (PolyQueue _ _ p) = p!!nth
