#!/usr/bin/env stack
-- stack runghc --package reanimate
module Main(main) where

import Reanimate
import Reanimate.Builtin.Documentation

main :: IO ()
main = reanimate $ docEnv $ scene $ do
  s1 <- newSpriteSVG $ withFillOpacity 1 $ withFillColor "blue" $ mkCircle 3
  _ <- newSpriteSVG $ withFillOpacity 1 $ withFillColor "red" $ mkRect 8 3
  wait 1
  spriteZ s1 1
  wait 1
