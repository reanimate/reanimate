#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Graphics.SvgTree (Tree)
import           Reanimate

main :: IO ()
main = reanimate bbox

bbox :: Animation
bbox = bg `parA`
    mapA (translate (-screenWidth/4) 0) bbox1 `parA`
    mapA (translate (screenWidth/4) 0) bbox2
  where
    bg = animate $ const $ mkBackground "black"

bbox1 :: Animation
bbox1 = mkAnimation 5 $ \t ->
    mkGroup
      [ mkBoundingBox $ rotate (360*t) svg
      , withFillColor "white" $ rotate (360*t) svg ]
  where
    svg = scale 2 $ center $ latexAlign "\\sum_{k=1}^\\infty"

bbox2 :: Animation
bbox2 = playThenReverseA $ mkAnimation 2.5 $ \t ->
  mkGroup
    [ mkBoundingBox $ partialSvg t heartShape
    , withStrokeColor "white" $ withFillOpacity 0 $
      partialSvg t heartShape ]

mkBoundingBox :: Tree -> Tree
mkBoundingBox svg = withStrokeColor "red" $ withFillOpacity 0 $
    translate (x+w/2) (y+h/2) $
    mkRect w h
  where
    (x, y, w, h) = boundingBox svg

heartShape :: Tree
heartShape = lowerTransformations $ scaleXY 1 (-1) $ scale 0.1 $
    center $ rotateAroundCenter 225 $ mkPathString
      "M0.0,40.0 v-40.0 h40.0\
      \a20.0 20.0 90.0 0 1 0.0,40.0\
      \a20.0 20.0 90.0 0 1 -40.0,0.0 Z"
