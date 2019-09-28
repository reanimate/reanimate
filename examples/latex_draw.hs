#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Reanimate

main :: IO ()
main = reanimate $
    bg `parA` (playThenReverseA $ drawText `andThen` fillText)
  where
    bg = animate $ const $ mkBackground "black"
    msg = "\\sum_{k=1}^\\infty {1 \\over k^2} = {\\pi^2 \\over 6}"
    glyphs = withStrokeWidth 0.01 $ center $ latexAlign msg
    fillText = mkAnimation 1 $ \t ->
      scale 2 $ withFillColor "white" $ withFillOpacity t glyphs
    drawText = mkAnimation 2 $ \t ->
      scale 2 $
        withStrokeColor "white" $ withFillOpacity 0 $
          partialSvg t glyphs
