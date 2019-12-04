#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main (main) where

import           Reanimate
--import           Reanimate.Povray
import           Reanimate.Raster
import           Data.String.Here
import Data.Text (Text)

main :: IO ()
main = reanimate $ mkAnimation 5 $ \t ->
    let s = fromToS 1 4 t in
    mkGroup
    [ mkBackground "black"
    , povray [] (script (svgAsPngFile texture) s) ]
  where

texture :: SVG
texture = checker 10 10

script :: FilePath -> Double -> Text
script png s = [iTrim|
//EXAMPLE OF SPHERE

//Files with predefined colors and textures
#include "colors.inc"
#include "glass.inc"
#include "golds.inc"
#include "metals.inc"
#include "stones.inc"
#include "woods.inc"

#include "shapes3.inc"

//Place the camera
camera {
  //orthographic
  perspective
  // angle 50
  location <0,-${max 0 (((s**1.5)-1)/16)},-3>
  look_at  <0,0,0>
  //right x*image_width/image_height
  up <0,9,0>
  right <16,0,0>
}



//Ambient light to "brighten up" darker pictures
global_settings { ambient_light White*3 }

//Set a background color
//background { color White }
//background { color rgbt <0.1, 0, 0, 0> } // red
background { color rgbt <0, 0, 0, 1> } // transparent

polygon {
  4,
  //<-9, -4.5>, <-9, 4.5>, <9, 4.5>, <9, -4.5>
  <0, 0>, <0, 1>, <1.777, 1>, <1.777, 0>
  texture {
    //pigment{ color rgb <0,0,1> }
    pigment{
      image_map{ png ${png} }
    }
  }
  translate <-1.777/2,-0.5>
  scale 9
}

             |]

checker :: Int -> Int -> SVG
checker w h =
  withFillColor "white" $
  withStrokeColor "white" $
  withStrokeWidth 0.1 $
  mkGroup
  [ withStrokeWidth 0 $
    withFillOpacity 1 $ mkBackground "blue"
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

