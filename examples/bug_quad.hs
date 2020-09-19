#!/usr/bin/env stack
-- stack runghc --package reanimate
module Main(main) where

import Reanimate
import Reanimate.Builtin.Documentation
import Reanimate.Morph.Common
import Reanimate.Morph.Linear

main :: IO ()
main = reanimate $ docEnv $ scene $ do
    play $ animate $ \t -> morph rawLinear src dst t
  where
    src = mkPathString "M 0 0 q 1 1, 2 0 z"
    dst = mkPathString "M 0 0 q 1 2, 2 0 z"
