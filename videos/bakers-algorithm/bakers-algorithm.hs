#!/usr/bin/env stack
-- stack --resolver lts-13.14 runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import           Control.Lens

import           Graphics.SvgTree
import           Reanimate.Driver (reanimate)
import           Reanimate.LaTeX
import           Reanimate.Animation
import           Reanimate.Svg
import           Reanimate.Signal

{- Script

Computer memory is a finite resource: If it is not reused, it'll eventually
run out.



-}

-- screen width 320
-- screen height 180
{-
[flat (1/3) 0, linear (1/3) 0 1, linear (1/3) 1 0]
-}
main :: IO ()
main = reanimate $ pauseAtEnd 2
  (animate $ const $ mkBackground "black") `sim`
  drawBox

drawBox :: Animation
drawBox = mkAnimation 5 $ \t ->
  mkGroup
  [ withFillColor "white" $
    translate 0 (-70) $
    scale 2 $ center $ latex "Baker's Algorithm"
  , let s    = signalFromList    [(0.7, signalFlat 0), (1, signalLinear)] t
        d    = signalFromList    [(0.7, signalFlat 0), (1, signalLinear)] t
        draw = signalFromList [(0.5, signalLinear), (1, signalFlat 1)] t
        mlc  = MemoryLineChart
               { mlcWidth = 230
               , mlcHeight = 50 + s*50
               , mlcDivider = d
               , mlcOuterBox = draw }
    in translate 0 20 $ renderMemoryLineChart mlc
  ]

highlightBox :: Animation
highlightBox = mkAnimation 2 $ \t ->
  mkGroup
  [ withFillColor "white" $
    translate 0 (-70) $
    scale 2 $ center $ latex "Highlightbox"
  , let boxX = negate mlcWidth / 2
        boxY = negate mlcHeight / 2
        mlcWidth = 230
        mlcHeight = 50
        s = signalFromList [(0.0, signalFlat 0), (1, signalBell 2)] t
    in
    withStrokeColor "white" $
    withStrokeWidth (Num $ 0.5 + s) $
    withFillOpacity 0 $
    translate (boxX + mlcWidth/2) (boxY + mlcHeight/2) $
    mkRect (Num mlcWidth) (Num mlcHeight)
  ]

data MemoryLineChart = MemoryLineChart
  { mlcWidth  :: Double
  , mlcHeight :: Double
  , mlcDivider :: Double -- 0 -> 1
  , mlcOuterBox :: Double -- 0 -> 1
  }

mlcBox :: MemoryLineChart -> (Double, Double, Double, Double)
mlcBox MemoryLineChart{..} = (boxX, boxY, mlcWidth, mlcHeight)
  where
    boxX = negate mlcWidth / 2
    boxY = negate mlcHeight / 2

renderMemoryLineChart :: MemoryLineChart -> Tree
renderMemoryLineChart MemoryLineChart{..} = mkGroup
    [ withStrokeColor "white" $
      withStrokeWidth (Num 0.5) $
      withFillOpacity 0 $
      partialSvg mlcOuterBox $ pathify $
      translate (boxX + mlcWidth/2) (boxY + mlcHeight/2) $
      mkRect (Num mlcWidth) (Num mlcHeight)
    , withStrokeColor "white" $
      withStrokeWidth (Num 0.5) $
      mkLine (Num (negate $ mlcWidth/2), Num 0)
             (Num (negate (mlcWidth/2) + lineWidth), Num 0)
    , withFillColor "white" $
      translate (negate (mlcWidth/2) - 10) 0 $ rotate (-90) $
      center $ latex "Memory"
    , withFillColor "white" $
      translate 0 (mlcHeight/2 + 10) $
      center $ latex "Time $\\rightarrow$"
    ]
  where
    boxX = negate mlcWidth / 2
    boxY = negate mlcHeight / 2
    lineWidth = mlcDivider * mlcWidth
