#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp  #-}
module Main(main) where

import           Codec.Picture
import           Control.Lens           ((&))
import           Reanimate
import           Reanimate.Morph.Common
import           Reanimate.Morph.Linear

bgColor :: PixelRGBA8
bgColor = PixelRGBA8 252 252 252 0xFF

main :: IO ()
main = reanimate $
  addStatic (mkBackgroundPixel bgColor) $
  mapA (withStrokeWidth 0) $
  mapA (withStrokeColor "black") $
  mapA (withFillOpacity 1) $
    scene $ do
      play $ step stage1 stage2
      play $ step stage2 stage3
      play $ step stage3 stage4
      play $ step stage4 stage1
        & setDuration 5
  where
    radius = 2.5
    step from to =
      signalA (curveS 4) $ animate $ morph linear from to
    stage1 = translate (-3) 0 $ withFillColor "red" $ mkCircle radius
    stage2 = translate 3 0 $ withFillColor "blue" $ mkRect (radius*2) (radius*2)
    stage3 = mkGroup
      [translate (-1) (-1) $ withFillColor "green" $ mkRect (radius*0.5) (radius*0.5)
      ,translate 1 (1) $ withFillColor "black" $ mkRect (radius*0.5) (radius*0.5) ]
    stage4 = translate (-3) 0 $ withFillColor "purple" $ mkCircle (radius*0.24)
