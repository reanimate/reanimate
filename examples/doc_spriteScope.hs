#!/usr/bin/env stack
-- stack runghc --package reanimate
module Main(main) where

import Reanimate
import Reanimate.Builtin.Documentation

main :: IO ()
main = reanimate $ docEnv $ scene $ do
  -- the rect lives through the entire 3s animation
  newSpriteSVG_ $ translate (-3) 0 $ mkRect 4 4
  wait 1
  spriteScope $ do
    -- the circle only lives for 1 second.
    local <- newSpriteSVG $ translate 3 0 $ mkCircle 2
    spriteE local $ overBeginning 0.3 fadeInE
    spriteE local $ overEnding 0.3 fadeOutE
    wait 1
  wait 1
