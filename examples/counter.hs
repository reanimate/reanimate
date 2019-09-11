#!/usr/bin/env stack
-- stack --resolver lts-13.14 runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import           Control.Lens
import           Data.Text (pack, Text)
import Numeric

import           Graphics.SvgTree hiding (Text)
import           Reanimate.Driver (reanimate)
import           Reanimate.LaTeX
import           Reanimate.Monad
import           Reanimate.Svg
import           Reanimate.Signal

main :: IO ()
main = reanimate $ mkAnimation 2 $ do
  s <- getSignal $ signalFromTo 0 (2*60-1) signalLinear
  emit $ mkCircle (Num s)
