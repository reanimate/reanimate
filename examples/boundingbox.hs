#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Lens

import           Graphics.SvgTree (Number(..),Tree)
import           Reanimate.Driver (reanimate)
import           Reanimate.LaTeX
import           Reanimate.Monad
import           Reanimate.Svg
import           Reanimate.Signal
import           Reanimate.Constants

main :: IO ()
main = reanimate bbox

bbox :: Animation
bbox = bg `sim`
    mapA (translate (-screenWidth/4) 0) bbox1 `sim`
    mapA (translate (screenWidth/4) 0) bbox2
  where
    bg = mkAnimation 0 $ emit $ mkBackground "black"

bbox1 :: Animation
bbox1 = mkAnimation 5 $ do
    s <- getSignal signalLinear
    emit $ mkGroup
      [ mkBoundingBox $ rotate (360*s) svg
      , withFillColor "white" $ rotate (360*s) svg ]
  where
    svg = scale 2 $ center $ latexAlign "\\sum_{k=1}^\\infty"

bbox2 :: Animation
bbox2 = autoReverse $ mkAnimation 2.5 $ do
  s <- getSignal signalLinear
  emit $ mkGroup
    [ mkBoundingBox $ partialSvg s heartShape
    , withStrokeColor "white" $ withFillOpacity 0 $
      partialSvg s heartShape ]

mkBoundingBox :: Tree -> Tree
mkBoundingBox svg = withStrokeColor "red" $ withFillOpacity 0 $
    translate (x+w/2) (y+h/2) $
    mkRect (Num w) (Num h)
  where
    (x, y, w, h) = boundingBox svg

heartShape = lowerTransformations $ scaleXY 1 (-1) $ scale 0.1 $
    center $ rotateAroundCenter 225 $ mkPathString
      "M0.0,40.0 v-40.0 h40.0\
      \a20.0 20.0 90.0 0 1 0.0,40.0\
      \a20.0 20.0 90.0 0 1 -40.0,0.0 Z"
