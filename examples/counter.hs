#!/usr/bin/env stack
-- stack runghc --package reanimate
module Main (main) where

import           Graphics.SvgTree hiding (Text)
import           Reanimate

main :: IO ()
main = reanimate $ mkAnimation dur $
    mkCircle . signalFromTo 0 (dur*60-1) signalLinear
  where
    dur = 2
