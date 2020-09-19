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
{-
main = reanimate $ docEnv $ mapA (withFillOpacity 1) $ sceneAnimation $ do
  cam <- newObject Camera

  newSpriteSVG_ $ withStrokeWidth (defaultStrokeWidth*0.5) $ mkGroup
    [ mkLine (0, screenBottom) (0, screenTop)
    , mkLine (screenLeft, 0) (screenRight, 0)
    ]

  box <- newObject $ Rectangle 1 1
  cameraAttach cam box
  oModify box $ oContext .~ withFillColor "green"
  oModify box $ oMargin .~ (0,0,0,0)
  oModify box $ oLeftX .~ 0
  oModify box $ oBottomY .~ 0
  oModify box $ oContext .~ withStrokeWidth 0

  box2 <- newObject $ Rectangle 1 1
  cameraAttach cam box2
  oModify box2 $ oContext .~ withFillColor "blue"
  oModify box2 $ oMargin .~ (0,0,0,0)
  oModify box2 $ oCenterXY .~ (-2,2)
  oModify box2 $ oContext .~ withStrokeWidth 0

  dot <- newObject $ Circle 0.05
  cameraAttach cam dot
  oModify dot $ oContext .~ withFillColor "red"
  oModify dot $ oMargin .~ (0,0,0,0)
  oModify dot $ oCenterXY .~ (0.5,0.5)
  oModify dot $ oContext .~ withStrokeWidth 0

  dot2 <- newObject $ Circle 0.05
  cameraAttach cam dot2
  oModify dot2 $ oContext .~ withFillColor "red"
  oModify dot2 $ oMargin .~ (0,0,0,0)
  oModify dot2 $ oCenterXY .~ (-2,2)
  oModify dot2 $ oContext .~ withStrokeWidth 0
  
  oShow box
  oShow box2
  oShow dot
  oShow dot2

  wait 1

  cameraSetFocus cam (0.5, 0.5)
  waitOn $ do
    fork $ cameraPan cam 1 (0.5, 0.5)
    fork $ cameraZoom cam 1 2
  wait 1
  cameraSetFocus cam (-2,2)
  cameraZoom cam 0.5 1.5
  cameraZoom cam 0.5 2
  wait 1
  -- cameraSetFocus cam (-0.5,0.5)
  waitOn $ do
    fork $ cameraPan cam 1 (-2, 2)
    fork $ cameraZoom cam 1 0.5
  -- oTweenS cam 2 $ \t -> do
  --   oScale %= \v -> fromToS v 3 t
  
  -- cameraSetFocus cam (0.5,0.5)
  
  -- oTweenS cam 1 $ \t -> do
  --   oScale %= \v -> fromToS v 1 t
  -- wait 4
  -- oTweenS cam 2 $ \t -> do
  --   oScaleOrigin .= (fromToS 0.5 (-2) t, fromToS 0.5 2 t)
  --   oScale .= fromToS 2 1 t
  --   oTranslate . _1 %= \v -> fromToS v (-2) t
  --   oTranslate . _2 %= \v -> fromToS v 2 t
  {-
    Origin: (-2,2)
    Translate: (3, -1)
    New origin: (0.5, 0.5)

    (0.5, 0.5)
    (0.5+2, 0.5-2)
    (2.5, -1.5)      origin move
    (2.5*3, -1.5*3)
    (7.5, -4.5)      scale
    (7.5-2, -4.5+2)
    (5.5, -2.5)      origin move back
    (5.5-3, -2.5+1)
    (2.5, -1.5)      translate

    (0.5-2.5, 0.5-(-1.5))
    (-2, 2)
  -}

  wait 1
-}
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
