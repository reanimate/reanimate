#!/usr/bin/env stack
-- stack runghc --package reanimate
module Main(main) where

import Reanimate
import Reanimate.Builtin.Documentation

main :: IO ()
main = reanimate $ docEnv $ scene $ do
  v <- newVar 0
  _ <- newSprite $ mkCircle <$> unVar v
  writeVar v 1; wait 1
  writeVar v 2; wait 1
  writeVar v 3; wait 1
