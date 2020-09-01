#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import Reanimate
import Reanimate.Scene
import Reanimate.Builtin.Documentation
import Control.Lens

main :: IO ()
-- main = reanimate $ docEnv $ oDraw $
--   withFillOpacity 1 $ withStrokeWidth (defaultStrokeWidth*0) $ center $ scale 2 $ latex "oDraw"
main = reanimate $ docEnv $ scene $ do
  txt <- oNew $ withStrokeWidth 0 $ withFillOpacity 1 $
    center $ scale 4 $ latex "oDraw"
  oModify txt $ oEasing .~ id
  oShowWith txt oDraw
  wait 1
  oFadeOut txt 1
