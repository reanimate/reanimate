#!/usr/bin/env stack
-- stack runghc --package reanimate
module Main (main) where

import           Reanimate
import           Reanimate.GeoProjection
import           Reanimate.Builtin.Images

main :: IO ()
main = reanimate $ animate $
  const $ scaleToSize screenWidth screenHeight $
  embedImage $ project smallEarth mollweideP
