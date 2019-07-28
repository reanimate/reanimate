#!/usr/bin/env stack
-- stack --resolver lts-13.14 runghc --package reanimate
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
main = reanimate $
    bg `sim` (autoReverse $ drawText `andThen` fillText)
  where
    bg = mkAnimation 0 $ emit (mkBackground "black")
    msg = "\\sum_{k=1}^\\infty {1 \\over k^2} = {\\pi^2 \\over 6}"
    glyphs = pathify $ center $ latexAlign msg
    fillText = mkAnimation 1 $ do
      s <- getSignal signalLinear
      emit $ scale 5 $ withFillColor "white" $ withFillOpacity s glyphs
    drawText = mkAnimation 2 $ do
      s <- getSignal signalLinear
      emit $ scale 5 $
        withStrokeColor "white" $ withFillOpacity 0 $ withStrokeWidth (Num 0.1) $
          partialSvg s glyphs
