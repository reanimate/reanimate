#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Main (main) where

import           Codec.Picture
import           Codec.Picture.Types
import           Control.Lens          ((^.))
import           Control.Monad
import           Data.Monoid
import           Data.String.Here
import           Data.Text             (Text)
import           Graphics.SvgTree      hiding (Text)
import           Reanimate
import           Reanimate.Animation
import           Reanimate.Effect
import           Reanimate.Povray
import           Reanimate.Raster
import           Reanimate.Scene
import           System.Random
import           System.Random.Shuffle


main :: IO ()
main = reanimate $ parA bg $ sceneAnimation $ do
    xRot <- newVar (-45)
    yRot <- newVar 220
    _ <- newSprite $ do
      getX <- freezeVar xRot
      getY <- freezeVar yRot
      return $ \real_t dur t ->
        povray [] $
        script (svgAsPngFile (texture (t/dur))) (getX real_t) (getY real_t) 0
    --wait 2
    let tDuration = 3
    tweenVar yRot tDuration (\t v -> fromToS v (v+180) $ curveS 2 (t/tDuration))
    tweenVar xRot (tDuration/2) (\t v -> fromToS v (v+90) $ curveS 2 (t/(tDuration/2)))
    fork $ do
      wait (tDuration/2)
      tweenVar xRot (tDuration/2) (\t v -> fromToS v (v-90) $ curveS 2 (t/(tDuration/2)))
    wait tDuration
    --wait 2
  where
    bg = animate $ const $ mkBackgroundPixel $ PixelRGBA8 252 252 252 0xFF

texture :: Double -> SVG
texture t = mkGroup
  [ checker 20 20
  ]

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


