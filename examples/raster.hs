#!/usr/bin/env stack
-- stack runghc --package reanimate
module Main (main) where

import           Reanimate
import           Codec.Picture

main :: IO ()
main = reanimate $ mkAnimation 5 $ \t ->
    mkGroup
      [ mkBackground "black"
      , rotate (t*360) $ scaleToWidth 6 $ embedImage img
      ]
  where
    img = generateImage pixelRenderer 255 255
    pixelRenderer x y = PixelRGB8 (fromIntegral x) (fromIntegral y) 128
