{-# LANGUAGE RecordWildCards #-}
module Reanimate.Math.Triangulate where

import           Control.Monad
import           Data.Hashable
import           Data.List             (intersect, nub, tails)
import           Data.Ratio
import           Data.Serialize
import qualified Data.Set              as Set
import           Data.Vector           (Vector)
import qualified Data.Vector           as V
import qualified Data.Vector.Mutable   as MV
import           Linear.Matrix         (det33)
import           Linear.Metric
import           Linear.V2
import           Linear.V3
import           Linear.Vector
import           Reanimate.Math.Common
import           Control.Monad.ST

-- Max edges: n-2
-- Each edge is represented twice: 2n-4
-- Flat structure:
--   edges   :: V.Vector Int -- max length (2n-4)
--   offsets :: V.Vector Int -- length n
-- Combine the two vectors? < n => offsets, >= n => edges?
type Triangulation = V.Vector [Int]

-- FIXME: Move to Common or a Triangulation module
-- O(n)
edgesToTriangulation :: Int -> [(Int,Int)] -> Triangulation
edgesToTriangulation size edges = runST $ do
  v <- MV.replicate size []
  forM_ edges $ \(e1,e2) -> do
    MV.modify v (e1:) e2
    MV.modify v (e2:) e1
  forM_ [0..size-1] $ \i ->
    MV.modify v (Set.toList . Set.fromList) i
  V.unsafeFreeze v
