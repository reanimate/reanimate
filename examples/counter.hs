#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import           Control.Lens
import           Data.Text (pack, Text)
import           Numeric

import           Graphics.SvgTree hiding (Text)
import           Reanimate.Driver (reanimate)
import           Reanimate.LaTeX
import           Reanimate.Monad
import           Reanimate.Svg
import           Reanimate.Signal

main :: IO ()
main = reanimate $ mkAnimation dur $ \t ->
    let s = signalFromTo 0 (dur*60-1) signalLinear t
    in mkCircle (Num s)
  where
    dur = 2
