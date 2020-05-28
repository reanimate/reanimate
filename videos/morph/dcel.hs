#!/usr/bin/env stack
-- stack --resolver lts-15.04 runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Lens                             ( )
import           Control.Monad
import           Data.Function
import           Data.List
import           Text.Printf
import           Data.List.NonEmpty                       ( NonEmpty )
import qualified Data.List.NonEmpty            as NE
import           Data.Maybe
import           Data.Ratio
import qualified Data.Text                     as T
import           Data.Tuple
import qualified Data.Vector                   as V
import           Debug.Trace
import           Linear.Matrix                     hiding ( trace )
import           Linear.Metric
import           Linear.V2
import           Linear.V3
import           Linear.Vector
import           Reanimate
import           Reanimate.Builtin.Documentation
import           Reanimate.Math.Balloon
import           Reanimate.Math.Common
import           Reanimate.Math.Polygon
import           Reanimate.Math.EarClip
import           Reanimate.Math.Smooth
import           Reanimate.Math.Render
import           Reanimate.Math.Compatible
import           Reanimate.Morph.Common
import           Reanimate.Morph.Linear
import           Reanimate.Morph.Rigid
import           Reanimate.Debug
import Reanimate.Math.DCEL as DCEL

-- p1 = centerPolygon $ shape2
--p1 = pScale 3.5 $ pAtCenter $ pAddPoints (0+2) (pSetOffset shape13 0)
-- p1 = setOffset (addPoints 2 shape13) 0
-- p2 = scalePolygon 0.5 $ centerPolygon shape20
-- p1 = centerPolygon shape2
--p2 = pScale 3.5 $ pAtCenter $ pAddPoints 0 (pSetOffset shape14 0)
-- p2 = setOffset shape14 0
p1 = pCopy p1'
p2 = pCopy p2'
(p1', p2') =
  -- normalizePolygons
             closestLinearCorrespondence
  -- leastWork defaultStretchCosts defaultBendCosts
  (pAtCenter $ unsafeSVGToPolygon 0.1 $ scale 4 $ latex "S")
  (pAtCenter $ unsafeSVGToPolygon 0.1 $ scale 4 $ latex "C")

(p1s,p2s) = unzip (compatiblyTriangulateP p1 p2)

main :: IO ()
main = reanimate $ sceneAnimation $ do
  -- newSpriteSVG_ $ mkBackground "black"
  newSpriteSVG_ $ mkBackgroundPixel rtfdBackgroundColor
  -- let m = buildMesh $ polygonMesh $ map (fmap realToFrac) $ V.toList $ polygonPoints p2
  let m = buildMesh $ polygonsMesh 
            (map (fmap realToFrac) $ V.toList $ polygonPoints p2)
            (map (map (fmap realToFrac) . V.toList . polygonPoints) p2s)
  -- newSpriteSVG_ $
  --   DCEL.renderMesh m
  newSpriteSVG_ $ scale 3 $ 
    mkGroup $ map polygonShape p2s
  newSpriteSVG_ $ scale 0.5 $ withFillColor "red" $
    mkGroup $ map (polygonDots . pScale 6) p2s
  -- newSpriteSVG_ $ withFillColor "red" $ scale 3 $ polygonNumDots p2
  wait 1
  return ()
 where
   

drawTrigs :: V.Vector (V2 Double) -> V.Vector RelTrig -> SVG
drawTrigs points trigs = mkGroup
  [ mkGroup
      [ withFillOpacity 1
        $ withStrokeWidth (defaultStrokeWidth * 0)
        $ withStrokeColor "grey"
        $ withFillColor "black"
        $ drawPolygon
        $ map (points V.!) [a, b, c]
      | (a, b, c) <- V.toList trigs
      ]
  ]

drawPoint :: V2 Double -> SVG
drawPoint (V2 x y) = translate x y $ mkCircle 0.1

drawPolygon :: [V2 Double] -> SVG
drawPolygon lst = mkLinePathClosed [ (x, y) | V2 x y <- lst ]
