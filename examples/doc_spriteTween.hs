#!/usr/bin/env stack
-- stack runghc --package reanimate
module Main(main) where

import Reanimate
import Reanimate.Builtin.Documentation

main :: IO ()
main = reanimate $ docEnv $ scene $ do
  s <- fork $ newSpriteA drawCircle
  spriteTween s 1 $ \val -> translate (screenWidth*0.3*val) 0
