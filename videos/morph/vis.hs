#!/usr/bin/env stack
-- stack --resolver lts-13.14 runghc --package reanimate
module Main where

import           Algorithms.Geometry.DelaunayTriangulation.DivideAndConquer (delaunayTriangulation)
import           Algorithms.Geometry.DelaunayTriangulation.Types
import           Codec.Picture.Types
import           Control.Lens
import           Control.Monad
import           Data.Function
import           Data.List
import           Data.Geometry.Point
import           Data.Ext
import Data.CircularList as CL
import           Data.List.NonEmpty                                         (NonEmpty)
import qualified Data.List.NonEmpty                                         as NE
import           Data.Maybe
import qualified Data.Text                                                  as T
import           Data.Tuple
import qualified Data.Vector                                                as V
import           Debug.Trace
import           Linear.Matrix                                              hiding
                                                                             (trace)
import           Linear.V2
import           Linear.V3
import           Linear.Vector
import           Linear.Metric
import           Numeric.LinearAlgebra                                      hiding
                                                                             (polar,
                                                                             scale,
                                                                             (<>))
import qualified Numeric.LinearAlgebra                                      as Matrix
import           Numeric.LinearAlgebra.HMatrix                              hiding
                                                                             (polar,
                                                                             scale,
                                                                             (<>))
import           Reanimate
import           Reanimate.Math.Visibility
import           Reanimate.Math.Common

main :: IO ()
main = reanimate $ setDuration 10 $ sceneAnimation $ do
  -- play $ renderPoly $ cyclePolygons shape2 !! 9
  forM_ (cyclePolygons shape2  ) $ play . renderPoly

renderPoly p'@(x:y:_) = addStatic (mkBackground "black") $ mkAnimation (distance x y) $ \t ->
  let p = interpFirst p' t in
  centerUsing outline $
  mkGroup
  [ outline
  , withFillColor "white" $ mkLinePathClosed
    [ (x,y) | V2 x y <- visibility p ]
  , withFillOpacity 0 $ outline
  , let V2 x y = head p in
    translate x y $ withFillColor "red" $ mkCircle 0.1
  -- , withFillColor "blue" $ latex $ T.pack $ show (t)
  ]
  where
    outline =
      {-withStrokeColor "grey" $-}
      withFillColor "grey" $ mkLinePathClosed
        [ (x,y) | V2 x y <- p' ++ [head p'] ]
