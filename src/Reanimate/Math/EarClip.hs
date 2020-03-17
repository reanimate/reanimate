module Reanimate.Math.EarClip where


import           Control.Monad
import           Control.Monad.ST
import           Data.List
import qualified Data.Set              as Set
import qualified Data.Vector           as V
import qualified Data.Vector.Mutable   as MV

import           Reanimate.Math.Common

-- import           Debug.Trace


-- FIXME: Move to Common or a Triangulation module
-- O(n)
edgesToTriangulation :: Polygon -> [(Int,Int)] -> Triangulation
edgesToTriangulation p edges = runST $ do
  v <- MV.replicate (length p) []
  forM_ edges $ \(e1,e2) -> do
    MV.modify v (e1:) e2
    MV.modify v (e2:) e1
  V.unsafeFreeze v

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
    -- worker :: Set.Set Int -> PolyQueue Int -> [(P,P)]
    worker _ears queue | isSimpleQ queue = []
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

data PolyQueue a = PolyQueue a [a] [a] [a]

sizeQ :: PolyQueue a -> Int
sizeQ (PolyQueue _ a b _) = 1 + length a + length b

mkQueue :: [a] -> PolyQueue a
mkQueue (x:xs) = PolyQueue x xs [] (reverse (x:xs))
mkQueue []     = error "mkQueue: empty"

toList :: PolyQueue a -> [a]
toList (PolyQueue e a b _) = e : a ++ b

isSimpleQ :: PolyQueue a -> Bool
isSimpleQ (PolyQueue _ xs ys _) =
  case xs ++ ys of
    [_,_] -> True
    _     -> False

peekQ :: PolyQueue a -> a
peekQ (PolyQueue e _ _ _) = e

nextQ :: PolyQueue a -> PolyQueue a
nextQ (PolyQueue x [] ys p)     =
  let (y:xs) = reverse (x:ys)
  in PolyQueue y xs [] (x:p)
nextQ (PolyQueue x (y:xs) ys p) = PolyQueue y xs (x:ys) (x:p)

dropQ :: PolyQueue a -> PolyQueue a
dropQ (PolyQueue _ [] ys p)    =
  let (x:xs) = reverse ys
  in PolyQueue x xs [] p
dropQ (PolyQueue _ (x:xs) ys p) = PolyQueue x xs ys p

prevQ :: Int -> PolyQueue a -> a
prevQ nth (PolyQueue _ _ _ p) = p!!nth
