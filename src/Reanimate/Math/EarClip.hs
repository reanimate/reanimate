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
  in worker dcel ears (cycle [0 .. dcelSize dcel])
  where
    worker dcel ears [] = dcel
    worker dcel ears (x:xs)
      | x `Set.member` ears =
        let v1 = 
      | otherwise = worker dcel ears xs
