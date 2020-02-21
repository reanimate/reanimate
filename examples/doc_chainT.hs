#!/usr/bin/env stack
-- stack runghc --package reanimate
module Main(main) where

import Reanimate
import Reanimate.Transition
import Reanimate.Builtin.Documentation

main :: IO ()
main = reanimate $ docEnv $ chainT (overlapT 0.5 fadeT) [drawBox, drawCircle, drawProgress]
