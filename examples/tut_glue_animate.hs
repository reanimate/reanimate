#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import           Reanimate
import           Reanimate.Transition

import           Codec.Picture.Types
import           Data.Text           (Text)

bgColor :: PixelRGBA8
bgColor = PixelRGBA8 252 252 252 0xFF

segmentDuration :: Double
segmentDuration = 3

transitionTime :: Double
transitionTime = 0.5

main :: IO ()
main = reanimate $ bg `parA`
    chainT transition
    [animateCircleR, animateCircleP, animateRectR, animateColor
    ,signalA (constantS 0) $ setDuration transitionTime animateCircleR]
  where
    transition = overlapT transitionTime fadeT
    bg = animate $ const $ mkBackgroundPixel bgColor

animateCircleR :: Animation
animateCircleR = mkSegment "radius" $ \t -> mkCircle (t*2)

animateCircleP :: Animation
animateCircleP = mkSegment "drawn" $ \t ->
  withFillOpacity 0 $ partialSvg t (pathify $ mkCircle 2)

animateRectR :: Animation
animateRectR = mkSegment "rotation" $ \t -> rotate (t*360) $ mkRect 4 2

animateColor :: Animation
animateColor = mkSegment "color" $ \t ->
  withFillColorPixel (promotePixel $ turbo t) $ mkRect 4 2

mkSegment :: Text -> (Time -> SVG) -> Animation
mkSegment label gen = mkAnimation segmentDuration $ \t -> env $
  mkGroup
  [ gen t
  , withStrokeWidth 0 $ translate 0 3 $ scale 2 $
    center $ latex label ]

env :: SVG -> SVG
env =
  withStrokeColor "red" .
  withFillColor "black" .
  withStrokeWidth (defaultStrokeWidth*2)
