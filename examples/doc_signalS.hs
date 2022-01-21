#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import Reanimate
import Reanimate.Scene
import Reanimate.Builtin.Documentation
import Control.Lens

main :: IO ()
main = reanimate $ docEnv $ scene $ do
  signalS (curveS 2) $ play $ drawCircle
  signalS reverseS $ play $ drawCircle
