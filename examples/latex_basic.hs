#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Lens

import           Graphics.SvgTree (Number(..))
import           Reanimate.Driver (reanimate)
import           Reanimate.LaTeX
import           Reanimate.Animation
import           Reanimate.Svg
import           Reanimate.Signal

main :: IO ()
main = reanimate $ playThenReverseA $ mkAnimation 2 $ \t ->
    mkGroup
      [ mkBackground "black"
      , withStrokeColor "white" $ withFillOpacity 0 text
      , withFillColor "white" $ withFillOpacity t text
      ]
  where
    text = withStrokeWidth 0.01 $ scale 2 $ center $ latexAlign
      "\\sum_{k=1}^\\infty {1 \\over k^2} = {\\pi^2 \\over 6}"
