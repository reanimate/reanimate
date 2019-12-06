#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Main (main) where

import           Codec.Picture
import           Control.Monad
import           Data.String.Here
import           Data.Text             (Text)
import           Reanimate
import           Reanimate.Povray
import           Reanimate.Raster
import           Reanimate.Scene

main :: IO ()
main = reanimate $ parA bg $ sceneAnimation $ do
    play $ mkAnimation drawDuration $ \t -> partialSvg t (wireframe (-45) 220)
    xRot <- newVar (-45)
    yRot <- newVar 220
    -- _ <- newSprite $ do
    --   getX <- freezeVar xRot
    --   getY <- freezeVar yRot
    --   return $ \real_t dur t ->
    --     povraySlow [] $
    --     script (svgAsPngFile (texture (t/dur))) (getX real_t) (getY real_t) 0
    wf <- newSprite $ do
      getX <- freezeVar xRot
      getY <- freezeVar yRot
      return $ \real_t _dur _t ->
        wireframe (getX real_t) (getY real_t)
    --wait 2
    tweenVar yRot spinDur (\t v -> fromToS v (v+60*3) $ curveS 2 (t/spinDur))
    replicateM_ wobbles $ do
      tweenVar xRot (wobbleDur/2) (\t v -> fromToS v (v+90) $ curveS 2 (t/(wobbleDur/2)))
      fork $ do
        wait (wobbleDur/2)
        tweenVar xRot (wobbleDur/2) (\t v -> fromToS v (v-90) $ curveS 2 (t/(wobbleDur/2)))
      wait wobbleDur
    --wait 2
    destroySprite wf
    play $ mkAnimation drawDuration (\t -> partialSvg t (wireframe (-45) 220))
      # reverseA
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
  script (svgAsPngFile texture) rotX rotY 0

texture :: SVG
texture = checker 10 10

script :: FilePath -> Double -> Double -> Double -> Text
script png rotX rotY rotZ = [iTrim|
//Files with predefined colors and textures
#include "colors.inc"

#include "shapes3.inc"

//Place the camera
camera {
  orthographic
  location <0,0,-10>
  look_at  <0,0,0>
  up y*9
  right x*16
}
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
//background { color White }
//background { color rgbt <0.1, 0, 0, 0> } // red
background { color rgbt <0, 0, 0, 1> } // transparent

//Sphere with specified center point and radius
sphere {
  <0,0,0>, 3
  texture {
    uv_mapping pigment{
      image_map{ png ${png} }
    }
  }
  rotate <0,${rotY},${rotZ}>
  rotate <${rotX},0,0>
}

|]

checker :: Int -> Int -> SVG
checker w h =
  withStrokeColor "lightblue" $
  withStrokeWidth (defaultStrokeWidth/2) $
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
