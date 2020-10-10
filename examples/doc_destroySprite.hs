#!/usr/bin/env stack
-- stack runghc --package reanimate
module Main(main) where

import Reanimate
import Reanimate.Builtin.Documentation

main :: IO ()
main = reanimate $ docEnv $ scene $ do
  s <- newSpriteSVG $ withFillOpacity 1 $ mkCircle 1
  fork $ wait 1 >> destroySprite s
  play drawBox
