#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Reanimate
import           Reanimate.Builtin.Documentation
import qualified Data.Text as T

main :: IO ()
main = reanimate $ docEnv $ mkAnimation 2 $ \t ->
  scale 2 $
  withStrokeWidth 0.05 $
  mkText (T.take (round $ t*15) "text")

