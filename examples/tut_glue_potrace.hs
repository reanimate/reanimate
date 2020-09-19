#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Main (main) where

import           Reanimate
import           Reanimate.Povray  (povraySlow')

import           Codec.Picture
import           Control.Lens
import           Control.Monad
import           Data.Text         (Text)
import qualified Data.Text         as T
import           NeatInterpolation

main :: IO ()
main = reanimate $ parA bg $ scene $ do
    play $ mkAnimation drawDuration $ \t -> partialSvg t (wireframe (-45) 220)
    xRot <- newVar (-45)
    yRot <- newVar 220
    wf <- newSprite $ wireframe <$> unVar xRot <*> unVar yRot
    fork $ tweenVar yRot spinDur $ \v -> fromToS v (v+60*3) . curveS 2
    replicateM_ wobbles $ do
      tweenVar xRot (wobbleDur/2) $ \v -> fromToS v (v+90) . curveS 2
      tweenVar xRot (wobbleDur/2) $ \v -> fromToS v (v-90) . curveS 2
    destroySprite wf
    play $ mkAnimation drawDuration (\t -> partialSvg t (wireframe (-45) 220))
      & reverseA
  where
    drawDuration = 10
    wobbles = 3
    wobbleDur = 3
    spinDur = fromIntegral wobbles * wobbleDur
    bg = animate $ const $ mkBackgroundPixel $ PixelRGBA8 252 252 252 0xFF

wireframe :: Double -> Double -> SVG
wireframe rotX rotY =
  withStrokeColor "black" $
  withStrokeWidth (defaultStrokeWidth*0.2) $
  withFillOpacity 0 $
  lowerTransformations $
  flipYAxis $
  translate (-screenWidth/2) (-screenHeight/2) $
  scale (screenWidth/2560) $
  mkPath $ extractPath $
  vectorize_ ["-t","100"] $
  povraySlow' [] $
  script (svgAsPngFile texture) rotX rotY

texture :: SVG
texture = checker 10 10

script :: FilePath -> Double -> Double -> Text
script png rotX rotY =
  let png_ = T.pack png
      rotX_ = T.pack $ show rotX
      rotY_ = T.pack $ show rotY
  in [text|
#include "colors.inc"

//Place the camera
camera {
  perspective
  location <0,0,-9>
  look_at  <0,0,0>
  up y
  right x*16/9
}

//Ambient light to "brighten up" darker pictures
global_settings { ambient_light White*3 }

//Set a background color
background { color rgbt <0, 0, 0, 1> } // transparent

//Sphere with specified center point and radius
sphere {
  <0,0,0>, 3
  texture {
    uv_mapping pigment{
      image_map{ png "${png_}" }
    }
  }
  rotate <0,${rotY_},0>
  rotate <${rotX_},0,0>
}
|]

checker :: Int -> Int -> SVG
checker w h =
  withStrokeColor "lightblue" $
  withStrokeWidth (defaultStrokeWidth*4) $
  mkGroup
  [ withStrokeWidth 0 $
    withFillOpacity 0.8 $ mkBackground "white"
  , mkGroup
    [ translate (stepX*x-offsetX + stepX/2) 0 $
      mkLine (0, -screenHeight/2*0.9) (0, screenHeight/2*0.9)
    | x <- map fromIntegral [0..w-1]
    ]
  ,
    mkGroup
    [ translate 0 (stepY*y-offsetY) $
      mkLine (-screenWidth/2, 0) (screenWidth/2, 0)
    | y <- map fromIntegral [0..h]
    ]
  ]
  where
    stepX = screenWidth/fromIntegral w
    stepY = screenHeight/fromIntegral h
    offsetX = screenWidth/2
    offsetY = screenHeight/2
