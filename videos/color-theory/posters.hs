#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  )
where

import           System.Directory
import           Reanimate

import           Common

main :: IO ()
main = renameFile (svgAsPngFile monalisaPoster) "poster.png"

monalisaPoster :: SVG
monalisaPoster = mkGroup
  [ mkPic (-1) 1    viridis -- "viridis"
  , mkPic 0    1    cividis -- "cividis"
  , mkPic 1    1    parula  -- "parula"
  , mkPic (-1) 0    jet     -- "jet"
  , mkPic 0    0    inferno -- "inferno"
  , mkPic 1    0    sinebow -- "sinebow"
  , mkPic (-1) (-1) turbo   -- "turbo"
  , mkPic 0    (-1) plasma  -- "plasma"
  , mkPic 1    (-1) hsv     -- "hsv"
  ]
 where
  mkPic x y cm =
    translate (screenWidth / 3 * x) (screenHeight / 3 * y) $ mkGroup
      [ scaleToSize (screenWidth / 3) (screenHeight / 3)
        $ embedImage
        $ applyColorMap cm monalisa
      ]
