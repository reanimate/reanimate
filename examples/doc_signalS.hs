#!/usr/bin/env stack
-- stack runghc --package reanimate
module Main(main) where

import Reanimate
import Reanimate.Builtin.Documentation

main :: IO ()
main = reanimate $ docEnv $ playThenReverseA $ scene $ do
  sprite <- fork $ newSpriteA $ drawCircle
  signalS sprite 1 (curveS 2)
  wait 1
  signalS sprite 1 (powerS 2)
