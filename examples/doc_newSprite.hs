#!/usr/bin/env stack
-- stack runghc --package reanimate
module Main(main) where

import Reanimate
import Reanimate.Builtin.Documentation

main :: IO ()
main = reanimate $ docEnv $ scene $ do
  _ <- newSprite $ mkCircle <$> spriteT -- Circle sprite where radius=time.
  wait 2
