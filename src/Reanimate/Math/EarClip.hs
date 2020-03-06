module Reanimate.Math.EarClip where


import           Linear.V2
import qualified Data.Set as Set

import Reanimate.Math.Common
import Reanimate.Math.DCEL

-- Triangulation by ear clipping. O(n^2)
earClip :: [P] -> DCEL
earClip pts =
  let dcel = fromSimplePolygon pts
      ears = Set.fromList [ i
             | i <- [0..dcelSize dcel]
             , isEarCorner pts (getPoint (i-1) dcel) (getPoint i dcel) (getPoint (i+1) dcel) ]
  in worker dcel ears (mkQueue [0 .. dcelSize dcel])
  where
    worker dcel ears queue | isSimple queue = dcel
    worker dcel ears queue
      | x `Set.member` ears =
        let v0 = prev 2 queue
            v1 = prev 1 queue
            v3 = peek (next queue)
            v4 = peek (next $ next queue)
            e1 = isEarCorner (toList queue) v0 v1 v3
            e2 = isEarCorner (toList queue) v1 v3 v4
        in edge : worker dcel ears' (drop queue)
      | otherwise = worker dcel ears (next queue)
      where
        x = peek queue

data PolyQueue a = PolyQueue [a] [a] [a]

mkQueue :: [a] -> PolyQueue
mkQueue pts = PolyQueue pts [] (reverse pts)

toList :: PolyQueue a -> [a]
toList (PolyQueue a b _) = a ++ b

isSimple :: PolyQueue a -> Bool
isSimple (PolyQueue [_,_,_] _ _) = True
isSimple _ = False

peek :: PolyQueue a -> a
peek (PolyQueue (x:_) _ _) = x

next :: PolyQueue a -> PolyQueue a
next (PolyQueue [x] ys p) = PolyQueue (reverse (x:ys)) [] (x:p)
next (PolyQueue (x:xs) ys p) = PolyQueue xs (x:ys) (x:p)

drop :: PolyQueue a -> PolyQueue a
drop (PolyQueue [x] ys p) = PolyQueue (reverse ys) [] p
drop (PolyQueue (x:xs) ys p) = PolyQueue xs (x:ys) p

prev :: Int -> PolyQueue a -> a
prev nth (PolyQueue _ _ p) = p!!nth
