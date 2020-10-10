#!/usr/bin/env stack
-- stack runghc --package reanimate
module Main(main) where

import Reanimate
import Reanimate.Builtin.Documentation

main :: IO ()
main = reanimate $ docEnv $ scene $ do
  s <- fork $ newSpriteA drawBox
  v <- newVar 0
  applyVar v s rotate
  tweenVar v 2 $ \val -> fromToS val 90
