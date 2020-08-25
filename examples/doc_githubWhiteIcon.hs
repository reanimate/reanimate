#!/usr/bin/env stack
-- stack runghc --package reanimate
module Main (main) where

import           Reanimate
import           Reanimate.Builtin.Images

main :: IO ()
main = reanimate $ staticFrame 1 $ mkGroup
  [ mkBackground "grey"
  , githubWhiteIcon ]
