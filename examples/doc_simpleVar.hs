#!/usr/bin/env stack
-- stack runghc --package reanimate
module Main(main) where

import Reanimate
import Reanimate.Builtin.Documentation

main :: IO ()
main = reanimate $ docEnv $ scene $ do
  var <- simpleVar mkCircle 0
  tweenVar var 2 $ \val -> fromToS val (screenHeight/2)
