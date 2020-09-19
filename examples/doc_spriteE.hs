#!/usr/bin/env stack
-- stack runghc --package reanimate
module Main(main) where

import Reanimate
import Reanimate.Builtin.Documentation

main :: IO ()
main = reanimate $ docEnv $ scene $ do
  s <- fork $ newSpriteA drawCircle
  spriteE s $ overBeginning 1 fadeInE
  spriteE s $ overEnding 0.5 fadeOutE
