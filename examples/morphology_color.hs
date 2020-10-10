#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp  #-}
module Main(main) where

import           Codec.Picture
import           Reanimate
import           Reanimate.ColorComponents
import           Reanimate.Morph.Common
import           Reanimate.Morph.Linear

bgColor :: PixelRGBA8
bgColor = PixelRGBA8 252 252 252 0xFF

main :: IO ()
main = reanimate $
  addStatic (mkBackgroundPixel bgColor) $
  setDuration 5 $
  mapA (withStrokeWidth 0) $
  mapA (withFillOpacity 1) $
    scene $ do
      doMorph yellow blue
      doMorph blue yellow
  where
    doMorph from to =
      play $ pauseAtEnd 0.5 $ animate $ \t ->
        mkGroup
          [ translate (-screenWidth/2 + screenWidth/6*(n*2+1)) 0 $ mkGroup
            [ translate 0 3 $ scale 1.5 $ center $ latex label
            , translate 0 (-0.5) $ morph conf from to t
            ]
          | conf <- [ linear
                    , linear{morphColorComponents=xyzComponents}
                    , linear{morphColorComponents=hsvComponents}]
          | label <- ["LAB", "XYZ", "HSV"]
          | n <- [0..]
          ]
    radius = 2.5
    yellow = withFillColor "yellow" $ mkCircle radius
    blue   = withFillColor "blue" $ mkCircle radius
    -- green  = withFillColor "green" $ mkCircle radius
    -- pink   = withFillColor "pink" $ mkCircle radius
    -- red    = withFillColor "red" $ mkCircle radius
    -- purple = withFillColor "purple" $ mkCircle radius
