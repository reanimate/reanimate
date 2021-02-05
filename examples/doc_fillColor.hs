#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import Codec.Picture
import Control.Lens
import Reanimate
import Reanimate.Scene
import Reanimate.Builtin.Documentation

testSVG :: SVG
testSVG = withStrokeWidth 0
  $ withFillOpacity 1
  $ latex "(1,2)"

main :: IO ()
main = reanimate $ docEnv $ scene $ do
  test <- oNew testSVG
  oModifyS test $ do
    oStrokeColor .= (255 :: Pixel8, 0 :: Pixel8, 0 :: Pixel8)
    oFillColor .= (255 :: Pixel8, 0 :: Pixel8, 0 :: Pixel8)
  oShowWith test oDraw
