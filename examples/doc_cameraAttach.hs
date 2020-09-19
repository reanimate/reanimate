#!/usr/bin/env stack
-- stack runghc --package reanimate
module Main(main) where

import Reanimate
import Reanimate.Builtin.Documentation
import Reanimate.Scene
import Control.Lens

main :: IO ()
main = reanimate $ docEnv $ scene $ do
  cam <- newObject Camera
  circ <- newObject $ Circle 2
  oModifyS circ $
    oContext .= withFillOpacity 1 . withFillColor "blue"
  oShow circ

  cameraAttach cam circ
  cameraZoom cam 2 2
  cameraZoom cam 2 1
