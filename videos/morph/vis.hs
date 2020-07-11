#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Codec.Picture.Types
import           Control.Lens                  ()
import           Control.Monad
import           Data.Function
import           Data.List
import           Data.List.NonEmpty            (NonEmpty)
import qualified Data.List.NonEmpty            as NE
import           Data.Maybe
import           Data.Ratio
import qualified Data.Text                     as T
import           Data.Tuple
import qualified Data.Vector                   as V
import           Debug.Trace
import           Linear.Matrix                 hiding (trace)
import           Linear.Metric
import           Linear.V2
import           Linear.V3
import           Linear.Vector
import           Numeric.LinearAlgebra         hiding (polar, scale, (<>))
import qualified Numeric.LinearAlgebra         as Matrix
import           Numeric.LinearAlgebra.HMatrix hiding (polar, scale, (<>))
import           Reanimate
import           Reanimate.Animation
import           Reanimate.Math.Balloon
import           Reanimate.Math.Common
import           Reanimate.Math.Triangulate
import           Reanimate.Math.Polygon
import           Reanimate.Math.EarClip
import           Reanimate.Math.SSSP
import           Reanimate.Math.Render
import           Reanimate.Math.Visibility
import           Reanimate.Math.Compatible
import           Reanimate.Morph.Common
import           Reanimate.PolyShape           (svgToPolygons)

p :: Polygon
p = mkPolygon $ V.fromList [V2 (0 % 1) (0 % 1),V2 (1 % 1) (0 % 1),V2 (1 % 1) (1 % 1),V2 (2 % 1) (1 % 1),V2 (2 % 1) ((-1) % 1),V2 (3 % 1) ((-1) % 1),V2 (3 % 1) (2 % 1),V2 (0 % 1) (2 % 1)]

pCuts' :: Polygon -> [(Int,Int)]
pCuts' p =
  [ (i, j)
  | i <- [0 .. pSize p-1 ]
  , j <- [i+2 .. pSize p-1 ]
  , (j+1) `mod` pSize p /= i
  , trace ("Check: " ++ show (i,j, pSize p)) $ pParent p i j == i ]

-- p :: Polygon
-- p = pScale 6 $ unsafeSVGToPolygon 0.1 $
--   lowerTransformations $ pathify $ center $ latex "$1$"

main :: IO ()
main = reanimate $ sceneAnimation $ do
  bg <- newSpriteSVG $ mkBackground "black"
  spriteZ bg (-1)
  newSpriteSVG_ $ translate 0 1 $ mkGroup
    [ withFillColor "grey" $ polygonShape p
    , polygonNumDots p
    ]
  forM_ (pCuts p) $ \(l,r) -> do
    play $ mkAnimation (1/60) $ \_ -> mkGroup
      [ translate (-3) 0 $ withFillColor "grey" $ polygonShape l
      , translate (-3) 0 $ polygonNumDots l
      , translate (3) 0 $ withFillColor "grey" $ polygonShape r
      , translate (3) 0 $ polygonNumDots r
      ]
  -- wait 1
  -- fork $ play $ drawTriangulation shape1 earCut'
  --   # mapA (translate (-3) 0)
  -- play $ drawTriangulation shape1 earClip'
  --   # mapA (translate (3) 0)
  return ()
