#!/usr/bin/env stack
-- stack runghc --package reanimate
module Main(main) where

import Codec.Picture
import Reanimate
import Reanimate.Builtin.Documentation

main :: IO ()
main = reanimate $ docEnv $ animate $ const $ showColorMap viridis
