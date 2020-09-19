#!/usr/bin/env stack
-- stack runghc --package reanimate
module Main(main) where

import Reanimate
import Reanimate.Builtin.Documentation
import Reanimate.Scene
import Control.Lens
import Linear.V2

main :: IO ()
main = reanimate $ docEnv $ scene $ do
  cam <- newObject Camera
  circ <- newObject $ Circle 2; oShow circ
  oModify circ $ oTranslate .~ V2 (-3) 0
  box <- newObject $ Rectangle 4 4; oShow box
  oModify box $ oTranslate .~ V2 3 0

  cameraAttach cam circ
  cameraAttach cam box
  cameraFocus cam (V2 (-3) 0)
  cameraZoom cam 2 2
  cameraZoom cam 2 1
  cameraFocus cam (V2 3 0)
  cameraZoom cam 2 2
  cameraZoom cam 2 1
