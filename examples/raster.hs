#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Lens

import           Graphics.SvgTree (Number(..))
import           Reanimate.Driver (reanimate)
import           Reanimate.LaTeX
import           Reanimate.Monad
import           Reanimate.Svg
import           Reanimate.Signal
import           Reanimate.Raster
import           Codec.Picture

main :: IO ()
main = reanimate $ mkAnimation 5 $ do
    s <- getSignal signalLinear
    emit $ mkGroup
      [ mkBackground "black"
      , rotate (s*360) $ center $ scaleToWidth 6 $ embedImage img
      ]
  where
    img = generateImage pixelRenderer 255 255
    pixelRenderer x y = PixelRGB8 (fromIntegral x) (fromIntegral y) 128
