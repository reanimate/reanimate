#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Data.Text                (Text)
import           Reanimate
import           Reanimate.Builtin.Images
import           Reanimate.Effect
import           Reanimate.Scene

bgColor :: String
bgColor = "white"

framePause :: Double
framePause = 3

transitionTime :: Double
transitionTime = 0.5

main :: IO ()
main = reanimate $ bg `parA` transitions fadeInE fadeOutE transitionTime
      [comp1, comp2, comp3, comp4, comp5, setDuration transitionTime comp1]
  where
    bg = animate $ const $ mkBackground bgColor
    comp1 = svgComponent "Circles" (mkCircle 2)
    comp2 = svgComponent "Rects" (mkRect 4 3)
    comp3 = svgComponent "Lines" (mkLine (-2,0) (2,0))
    comp4 = svgComponent "Images" (scale 0.5 svgLogo)
    comp5 = svgComponent "Paths" $
      withFillOpacity 0 $
      scale 8 $ withStrokeWidth (defaultStrokeWidth*0.3) $
      center $ latex "$\\pi$"

svgComponent :: Text -> SVG -> Animation
svgComponent txt svg = mkAnimation framePause $ const $
  mkGroup
  [ translate 0 (-1) $
    withStrokeWidth (defaultStrokeWidth*2) $
    withStrokeColor "red" $ withFillColor "black" svg
  , translate 0 3 $
    withFillColor "black" $ scale 2 $ center $ latex txt
  ]
