#!/usr/bin/env stack
-- stack --resolver lts-11.22 runghc --package reanimate
module Main (main) where

import           Control.Lens

import           Graphics.SvgTree (Number(..))
import           Reanimate.Driver (reanimate)
import           Reanimate.LaTeX  (latex)
import           Reanimate.Monad  (emit, mkAnimation)
import           Reanimate.Svg

main :: IO ()
main = reanimate $ mkAnimation 1 $ do
    emit $ mkBackground "black"
    emit $ withStrokeWidth (Num 0.1) $
      withStrokeColor "white" $
      withSubglyphs [0] (withFillColor "blue") $
      withSubglyphs [1] (withFillColor "yellow") $
      withSubglyphs [2] (withFillColor "green") $
      withSubglyphs [3] (withFillColor "red") $
      withSubglyphs [4] (withFillColor "darkslategrey") $
      svg
  where
    svg = scale 10 $ center $ latex "\\LaTeX"
