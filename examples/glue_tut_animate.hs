#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import Reanimate
import Reanimate.Builtin.Documentation
import Reanimate.Builtin.Images

main :: IO ()
main = reanimate $ bg `parA` (animateCircleR `seqA` animateCircleP `seqA` animateRectR)
    --(animateLogo)
  where
    bg = animate $ const $ mkBackground "white"

animateCircleR :: Animation
animateCircleR = mkAnimation 2 $ \t -> env $
  mkGroup
  [ mkCircle (t*2)
  , withStrokeWidth 0 $ translate 0 3 $ scale 2 $
    center $ latex "radius = t" ]

animateCircleP :: Animation
animateCircleP = mkAnimation 2 $ \t -> env $
  mkGroup
  [ partialSvg t (pathify $ mkCircle 2)
  , withStrokeWidth 0 $ translate 0 3 $ scale 2 $
    center $ latex "$\\% = t$" ]

animateRectR :: Animation
animateRectR = mkAnimation 2 $ \t -> env $
  mkGroup
  [ rotate (t*360) $ mkRect 4 2
  , withStrokeWidth 0 $ translate 0 3 $ scale 2 $
    center $ latex "rotation = t" ]

animateLogo :: Animation
animateLogo = mkAnimation 2 $ \t -> env $
  mkGroup
  [ partialSvg t haskellLogo
  , withStrokeWidth 0 $ translate 0 3 $ scale 2 $
    center $ latex "$\\% = t%" ]

env :: SVG -> SVG
env =
  withStrokeColor "red" .
  withFillColor "black" .
  withStrokeWidth (defaultStrokeWidth*2)
