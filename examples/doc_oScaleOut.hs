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
  txt <- oNew $ withStrokeWidth 0 $ withFillOpacity 1 $
    center $ scale 3 $ latex "oScaleOut"
  oFadeIn txt 1
  oHideWith txt $ adjustDuration (*2) . oScaleOut
