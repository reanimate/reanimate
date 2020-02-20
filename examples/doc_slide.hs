#!/usr/bin/env stack
-- stack runghc --package reanimate
module Main(main) where

import Reanimate
import Reanimate.Animation
import Reanimate.Builtin.Documentation
import Reanimate.Builtin.Slide
import Reanimate.Builtin.Flip

main :: IO ()
main = reanimate $ docEnv $ signalT (curveS 2) slide left right
  where
    left = drawCircle
    right = staticFrame 1 (withFillOpacity 1 $ mkBackground "black") `parA`
            mapA (flipXAxis . withStrokeColor "white") drawCircle
