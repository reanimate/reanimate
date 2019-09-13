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

main :: IO ()
main = reanimate $ autoReverse $ mkAnimation 2 $ do
    s <- getSignal signalLinear
    emit $ mkGroup
      [ mkBackground "black"
      , withStrokeColor "white" $ withFillOpacity 0 $ withStrokeWidth (Num 0.1) text
      , withFillColor "white" $ withFillOpacity s text ]
  where
    text = scale 4 $ center $ latexAlign
      "\\sum_{k=1}^\\infty {1 \\over k^2} = {\\pi^2 \\over 6}"
