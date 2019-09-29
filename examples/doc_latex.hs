#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Reanimate
import           Reanimate.Builtin.Documentation

main :: IO ()
main = reanimate $ docEnv $ animate $ const $
  withStrokeWidth 0 . withFillOpacity 1 . scale 3 . center $
  latex "$e^{i\\pi}+1=0$"
