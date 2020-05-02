module Reanimate.Math.EarClip
  ( earClip
  , earClip'
  ) where

import           Data.List
import qualified Data.Set                   as Set
import qualified Data.Vector                as V
import           Linear.V2

import           Reanimate.Math.Common
import           Reanimate.Math.Triangulate

-- Triangulation by ear clipping. O(n^2)
earClip :: (Fractional a, Ord a) => V.Vector (V2 a) -> Triangulation
earClip = last . earClip'

earClip' :: (Fractional a, Ord a) => V.Vector (V2 a) -> [Triangulation]
earClip' p = map (edgesToTriangulation $ V.length p) $ inits $
  let ears = Set.fromList [ i
             | i <- elts
             , isEarCorner p elts (mod (i-1) n) i (mod (i+1) n) ]
  in worker ears (mkQueue elts)
  where
    n = V.length p
    elts = [0 .. n-1]
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

-- sizeQ :: PolyQueue a -> Int
-- sizeQ (PolyQueue _ a b _) = 1 + length a + length b

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

-- O(n)
-- Returns true if ac can be cut from polygon. That is, true if 'b' is an ear.
-- isEarCorner polygon a b c = True iff ac can be cut
isEarCorner :: (Fractional a, Ord a) => V.Vector (V2 a) -> [Int] -> Int -> Int -> Int -> Bool
isEarCorner p polygon a b c =
    isLeftTurn aP bP cP &&
    -- If it is a right turn then the line ac will be outside the polygon
    and [ not (isInside aP bP cP (p V.! k))
    | k <- polygon, k /= a && k /= b && k /= c
    ]
  where
    aP = p V.! a
    bP = p V.! b
    cP = p V.! c
