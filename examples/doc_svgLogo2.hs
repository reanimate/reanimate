#!/usr/bin/env stack
-- stack runghc --package reanimate

module Main (main) where

import Reanimate
import Reanimate.Builtin.Documentation
import Reanimate.External

main :: IO ()
main =
  reanimate $
    docEnv $
      staticFrame 1 $
        withFillOpacity 1 $ svgLogo "cassandra"
