#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import Reanimate
import Reanimate.Scene
import Reanimate.Builtin.Documentation

main :: IO ()
main = reanimate $ docEnv $ scene $ do
  txt <- oNew $ withStrokeWidth 0 $ withFillOpacity 1 $
    center $ scale 3 $ latex "oGrow"
  oShowWith txt oGrow
  wait 1; oHideWith txt oFadeOut
