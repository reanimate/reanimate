#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Main (main) where

import           Codec.Picture
import           Data.String.Here
import           Data.Text           (Text)
import           Reanimate
import           Reanimate.Animation
import           Reanimate.Effect
import           Reanimate.Povray
import           Reanimate.Signal
import           Reanimate.Raster
import           Reanimate.Svg

{- SCRIPT

Some ideas lend themselves well to be illustrated. Take spheres, for
example: It just so happens that the surface of a sphere is exactly 4 times the
area of a circle with the same radius.

Now, this relationship could be, and already have been, visually explored in mucher greater dept so
I'll leave it at this.
But there are countless other ideas and concepts that deserve to be illustrated yet haven't.
I want to remedy this, in part, by animating ideas I find interesting, but also, and perhaps more
importantly, by encouraging you to make your own animations. In the description of this video there
is a link to





-}

main :: IO ()
main = reanimate $ sceneAnimation $ do
  play $ drawSphere
    # setDuration 20
    # pauseAtEnd 5
  play $ rotateWireSphere
    # setDuration 2
    # signalA (powerS 2)
  fork $ play $ rotateWireSphere
    # setDuration 1
    # repeatA 10
    # takeA (2+5)
    # applyE (delayE 2 fadeOutE)
  fork $ play $ rotateSphere
    # setDuration 1
    # repeatA 10
    # applyE (overBeginning 2 $ constE $ withGroupOpacity 0)
    # applyE (delayE 2 $ overBeginning 5 fadeInE)

drawSphere :: Animation
drawSphere = animate $ \t ->
    partialSvg t $
    withStrokeColor "white" $
    withStrokeWidth 0.01 $
    withFillOpacity 0 $
    lowerTransformations $
    flipYAxis $
    translate (-screenWidth/2) (-screenHeight/2) $
    scale 0.00625 $
    mkPath $ extractPath $
    vectorize_ ["-i"] $
    povraySlow' [] (script (svgAsPngFile texture) 0)

rotateWireSphere :: Animation
rotateWireSphere = animate $ \t ->
    withStrokeColor "white" $
    withStrokeWidth 0.01 $
    withFillOpacity 0 $
    lowerTransformations $
    flipYAxis $
    translate (-screenWidth/2) (-screenHeight/2) $
    scale 0.00625 $
    mkPath $ extractPath $
    vectorize_ ["-i"] $
    povraySlow' [] (script (svgAsPngFile texture) (t*360/10))

rotateSphere :: Animation
rotateSphere = animate $ \t ->
    povraySlow [] (script (svgAsPngFile texture) (t*360/10))

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
  orthographic
  // angle 50
  location <0,0,-10>
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

//Sphere with specified center point and radius
sphere {
  <0,0,0>, 2
  texture {
    //pigment{ color rgbt <0,0,1,0.1> }
    uv_mapping pigment{
      image_map{ png ${png} }
      //color rgbt <0,0,1,0.1>
    }
  }
  rotate <0,${s'},0>
  rotate <-30,0,0>
}
             |]
  where
    precision = 0.1
    s' = fromIntegral (round (s / precision)) * precision

checker :: Int -> Int -> SVG
checker w h =
  withFillColor "white" $
  withStrokeColor "white" $
  withStrokeWidth 0.1 $
  mkGroup
  [ withFillOpacity 0.8 $ mkBackground "blue"
  , mkGroup
    [ translate (stepX*x-offsetX) 0 $
      mkLine (0, -screenHeight) (0, screenHeight)
    | x <- map fromIntegral [0..w-1]
    ]
  ,
    mkGroup
    [ translate 0 (stepY*y-offsetY) $
      mkLine (-screenWidth, 0) (screenWidth, 0)
    | y <- map fromIntegral [0..h-1]
    ]
  ]
  where
    stepX = screenWidth/fromIntegral w
    stepY = screenHeight/fromIntegral h
    offsetX = screenWidth/2
    offsetY = screenHeight/2
