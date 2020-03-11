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
edgesToTriangulation :: [P] -> [(Int,Int)] -> Triangulation
edgesToTriangulation p edges = V.fromList
  [ nub $ [ e2 | (e1,e2) <- edges, i == e1 ] ++ [ e1 | (e1,e2) <- edges, i == e2 ]
  | i <- [0.. length p-1]
  ]
  where
    pToI v = fromJust (elemIndex v p)

-- Triangulation by ear clipping. O(n^2)
earClip :: [P] -> Triangulation
earClip pts = edgesToTriangulation pts $
  let dcel = fromSimplePolygon pts
      ears = Set.fromList [ i
             | i <- [0..dcelSize dcel]
             , isEarCorner pts (getPoint (i-1) dcel) (getPoint i dcel) (getPoint (i+1) dcel) ]
  in worker dcel ears (mkQueue [0 .. dcelSize dcel-1])
  where
    -- worker :: DCEL -> Set.Set Int -> PolyQueue Int -> [(P,P)]
    worker dcel ears queue | isSimple queue = []
    worker dcel ears queue
      -- | trace (show (x, Set.member x ears)) False = undefined
      | x `Set.member` ears =
        let dq = dropQ queue
            v0 = getPoint (prevQ 1 queue) dcel
            v1i = prevQ 0 queue
            v1 = getPoint v1i dcel
            v3i = peekQ dq
            v3 = getPoint v3i dcel
            v4 = getPoint (peekQ (nextQ dq)) dcel
            e1 = if isEarCorner (map (flip getPoint dcel) $ toList dq) v0 v1 v3
                  then Set.insert v1i ears
                  else ears
            e2 = if isEarCorner (map (flip getPoint dcel) $ toList dq) v1 v3 v4
                  then Set.insert v3i e1
                  else e1
        in (v1i,v3i) : worker dcel e2 dq
      | otherwise = worker dcel ears (nextQ queue)
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
