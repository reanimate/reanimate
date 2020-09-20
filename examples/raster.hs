#!/usr/bin/env stack
-- stack runghc --package reanimate
module Main (main) where

import           Reanimate
import           Codec.Picture

main :: IO ()
main = reanimate $ addStatic bg $ mkAnimation 5 $ \t ->
    rotate (t*360) $ scaleToWidth 6 $ embedImage img
  where
    bg = mkBackground "black"
    img = generateImage pixelRenderer 255 255
    pixelRenderer x y = PixelRGB8 (fromIntegral x) (fromIntegral y) 128
