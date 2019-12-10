#!/usr/bin/env stack
-- stack runghc --package reanimate
module Main(main) where

import Codec.Picture
import Reanimate
import Reanimate.Builtin.Documentation

main :: IO ()
main = reanimate $ docEnv $ animate $ const $ mkColorMap magma

mkColorMap :: (Double -> PixelRGB8) -> SVG
mkColorMap f = center $ scaleToSize screenWidth screenHeight $ embedImage img
  where
    width = 256
    height = 1
    img = generateImage pixelRenderer width height
    pixelRenderer x _y = f (fromIntegral x / fromIntegral (width-1))
