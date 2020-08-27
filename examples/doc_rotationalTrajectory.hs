#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import Reanimate
import Reanimate.Builtin.Documentation
import Reanimate.Morph.Common
import Reanimate.Morph.Linear
import Reanimate.Morph.Rotational
import Graphics.SvgTree

main :: IO ()
main = reanimate $ docEnv $ playThenReverseA $ pauseAround 0.5 0.5 $ mkAnimation 3 $ \t ->
  withStrokeLineJoin JoinRound $
  let src = scale 8 $ center $ latex "X"
      dst = scale 8 $ center $ latex "H"
  in morph linear{morphTrajectory=rotationalTrajectory (0.5,0.5)} src dst t
