#!/usr/bin/env stack
-- stack runghc --package reanimate
module Main (main) where

import           Reanimate

main :: IO ()
main = reanimate $ mkAnimation dur $
    mkCircle . fromToS 0 (dur*60-1)
  where
    dur = 2
