#!/usr/bin/env stack
-- stack runghc --package reanimate
module Main(main) where

import Reanimate
import Reanimate.Builtin.Documentation

main :: IO ()
main = reanimate $ docEnv $ signalA (cubicBezierS (0.0, 0.8, 0.9, 1.0)) drawProgress
