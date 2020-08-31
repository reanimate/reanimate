#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import Reanimate
import Reanimate.Builtin.Documentation
import Reanimate.Math.Balloon
import Graphics.SvgTree

main :: IO ()
main = reanimate $ docEnv $ pauseAtEnd 1 $ mkAnimation 5 $ \t ->
  withStrokeLineJoin JoinRound $
  balloon (scale 8 $ center $ latex "X") t
