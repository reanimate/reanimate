{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK hide #-}
module Reanimate.Math.Triangulate
  ( Triangulation
  , edgesToTriangulation
  , edgesToTriangulationM
  , trianglesToTriangulation
  , trianglesToTriangulationM
  , triangulate
  )
where

import           Algorithms.Geometry.PolygonTriangulation.Triangulate (triangulate')
import           Algorithms.Geometry.PolygonTriangulation.Types
import           Control.Lens
import           Control.Monad
import           Control.Monad.ST
import           Data.Ext
import           Data.Geometry.PlanarSubdivision                      (PolygonFaceData)
import           Data.Geometry.Point
import           Data.Geometry.Polygon
import qualified Data.IntSet                                          as ISet
import qualified Data.PlaneGraph as Geo
import           Data.Proxy
import qualified Data.Vector                                          as V
import qualified Data.Vector.Mutable                                  as MV
import           Linear.V2
import           Reanimate.Math.Common
-- Max edges: n-2
-- Each edge is represented twice: 2n-4
-- Flat structure:
--   edges   :: V.Vector Int -- max length (2n-4)
--   offsets :: V.Vector Int -- length n
-- Combine the two vectors? < n => offsets, >= n => edges?
type Triangulation = V.Vector [Int]

-- FIXME: Move to Common or a Triangulation module
-- O(n)
edgesToTriangulation :: Int -> [(Int, Int)] -> Triangulation
edgesToTriangulation size edges = runST $ do
  v <- edgesToTriangulationM size edges
  V.unsafeFreeze v

edgesToTriangulationM :: Int -> [(Int, Int)] -> ST s (V.MVector s [Int])
edgesToTriangulationM size edges = do
  v <- MV.replicate size []
  forM_ edges $ \(e1, e2) -> do
    MV.modify v (e1 :) e2
    MV.modify v (e2 :) e1
  forM_ [0 .. size - 1] $ \i -> MV.modify v (ISet.toList . ISet.fromList) i
  return v

trianglesToTriangulation :: Int -> V.Vector (Int, Int, Int) -> Triangulation
trianglesToTriangulation size edges = runST $ do
  v <- trianglesToTriangulationM size edges
  V.unsafeFreeze v

trianglesToTriangulationM
  :: Int -> V.Vector (Int, Int, Int) -> ST s (V.MVector s [Int])
trianglesToTriangulationM size trigs = do
  v <- MV.replicate size []
  forM_ (V.toList trigs) $ \(a, b, c) -> do
    MV.modify v (\x -> b : c : x) a
    MV.modify v (\x -> a : c : x) b
    MV.modify v (\x -> a : b : x) c
  forM_ [0 .. size - 1] $ \i -> MV.modify v (ISet.toList . ISet.fromList) i
  return v


triangulate :: forall a. (Fractional a, Ord a) => Ring a -> Triangulation
triangulate r = edgesToTriangulation (ringSize r) ds
  where
    ds :: [(Int,Int)]
    ds =
      [ (a^.Geo.vData, b^.Geo.vData)
      | (d, Diagonal) <- V.toList (Geo.edges pg)
      , let (a,b) = Geo.endPointData d pg ]
    pg :: Geo.PlaneGraph () Int PolygonEdgeType PolygonFaceData a
    pg = triangulate' Proxy p
    p :: SimplePolygon Int a
    p = fromPoints $
      [ Point2 x y :+ n
      | (n,V2 x y) <- zip [0..] (V.toList (ringUnpack r)) ]
    -- ringUnpack
