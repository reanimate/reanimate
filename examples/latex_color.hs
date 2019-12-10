#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Reanimate

main :: IO ()
main = reanimate $ animate $ const $
    mkGroup
    [ mkBackground "black"
    , withStrokeColor "white" $
      withSubglyphs [0] (withFillColor "blue") $
      withSubglyphs [1] (withFillColor "yellow") $
      withSubglyphs [2] (withFillColor "green") $
      withSubglyphs [3] (withFillColor "red") $
      withSubglyphs [4] (withFillColor "darkslategrey")
      svg ]
  where
    svg = withStrokeWidth 0.01 $ scale 4 $ center $ latex "\\LaTeX"
