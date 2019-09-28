#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main (main) where

import           Control.Lens

import           Graphics.SvgTree (Number(..))
import           Reanimate.Driver (reanimate)
import           Reanimate.LaTeX
import           Reanimate.Monad
import           Reanimate.Svg
import           Reanimate.Signal
import           Reanimate.Raster
import           Reanimate.Povray
import           Codec.Picture
import           Data.String.Here

main :: IO ()
main = reanimate $ mkAnimation 5 $ \t ->
    let s = signalFromTo 0 360 signalLinear t in
    mkGroup
    [ mkBackground "black"
    , povray [] (script s) ]
  where
    script s = [iTrim|
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
  angle 50
  location <0,0,-10>
  look_at  <0,0,0>
  right x*image_width/image_height
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
    pigment{ color rgbt <0,0,1,0.1> }
  }
}

object {
  Ring_Sphere(2.00, 2.02, 0.015, 0.015, 12, 12)
  texture {
    pigment{ color<1,1,1> }
  }
  rotate <0,${s},0>
  rotate <-30,0,0>
}


             |]
