#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Reanimate
import           Reanimate.Builtin.Documentation

main :: IO ()
main = reanimate $ docEnv $ animate $ const $
  withStrokeWidth 0 . withFillOpacity 1 . scale 3 . center $
  latexAlign "R = \\frac{{\\Delta x}}{{kA}}"
