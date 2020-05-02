{-# LANGUAGE RecordWildCards #-}
module Reanimate.Math.Triangulate
  ( Triangulation
  , edgesToTriangulation
  ) where

import           Control.Monad
import qualified Data.Set              as Set
import qualified Data.Vector           as V
import qualified Data.Vector.Mutable   as MV
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
