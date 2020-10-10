#!/usr/bin/env stack
-- stack runghc --package reanimate
module Main(main) where

import Reanimate
import Reanimate.Animation
import Reanimate.Builtin.Documentation

main :: IO ()
main = reanimate $ docEnv $ scene $ do
  _ <- fork $ newSpriteA' SyncFreeze drawCircle
  play drawBox
  play $ reverseA drawBox
