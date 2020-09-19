#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import Reanimate
import Reanimate.Builtin.Documentation
import Reanimate.Scene
import Control.Lens
import Linear.V2

main :: IO ()
main = reanimate $ docEnv $ mapA (withFillOpacity 1) $ scene $ do
  cam <- newObject Camera

  txt <- newObject $ center $ latex "Fixed (non-cam)"
  oModify txt $
    oTopY .~ screenTop

  circle <- newObject $ Circle 1
  cameraAttach cam circle
  oModify circle $ oContext .~ withFillColor "blue"
  circleRight <- oRead circle oRightX

  box <- newObject $ Rectangle 2 2
  cameraAttach cam box
  oModify box $ oContext .~ withFillColor "green"
  oModify box $ oLeftX .~ circleRight
  boxCenter <- oRead box oCenterXY

  small <- newObject $ center $ latex "This text is very small"
  cameraAttach cam small
  oModifyS small $ do
    oCenterXY .= boxCenter
    oScale .= 0.1
  
  oShow txt
  oShow small
  oShow circle
  oShow box

  wait 1

  cameraFocus cam boxCenter
  waitOn $ do
    fork $ cameraPan cam 3 boxCenter
    fork $ cameraZoom cam 3 15
  
  wait 2
  waitOn $ do
    fork $ cameraZoom cam 3 1
  waitOn $ do
    fork $ cameraPan cam 1 (V2 0 0)
