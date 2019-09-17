#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Main (main) where

import           Control.Lens ()
import Control.Monad

import           Graphics.SvgTree (Number(..), Tree)
import           Reanimate.Driver (reanimate)
import           Reanimate.LaTeX  (latex)
import           Reanimate.Monad
import           Reanimate.Svg
import           Reanimate.ColorMap
import           Reanimate.Signal
import           Reanimate.Scene
import Codec.Picture.Types
import Data.Fixed (mod')

main :: IO ()
main = reanimate $ mkAnimation 0 (emit $ mkBackground "black") `sim`
  mainScene

o # f = f o

type Effect = Double -> Double -> Tree -> Tree

overBeginning :: Double -> Effect -> Effect
overBeginning maxT fn = \d t ->
  if t < maxT
    then fn maxT t
    else id

overEnding :: Double -> Effect -> Effect
overEnding minT fn d t =
  if t >= blankDur
    then fn minT (t-blankDur)
    else id
  where
    blankDur = d-minT

reverseE :: Effect -> Effect
reverseE fn = \d t -> fn d (d-t)

delayE :: Double -> Effect -> Effect
delayE delayT fn = \d t -> overEnding (d-delayT) fn d t

applyE :: Effect -> Animation -> Animation
applyE fn (Animation d genFrame) = Animation d $ do
  t <- askTime
  mapF (fn d t) genFrame

constE :: (Tree -> Tree) -> Effect
constE fn d t = fn

fadeInE :: Effect
fadeInE d t = withGroupOpacity (t/d)

fadeOutE :: Effect
fadeOutE = reverseE fadeInE

drawInE :: Effect
drawInE d t = withFillOpacity 0 . partialSvg (t/d) . pathify

drawOutE :: Effect
drawOutE = reverseE drawInE

fillInE :: Effect
fillInE d t = withFillOpacity f
  where
    f = t/d

askTime :: Frame Time
askTime = Frame $ \_dur t -> return t

freezeAtPercentage :: Double -> Animation -> Animation
freezeAtPercentage frac (Animation d genFrame) =
  Animation d $ Frame $ \_ _ -> unFrame genFrame d (d*frac)

{-mainScene :: Animation
mainScene = sceneAnimation $ mdo
  fork $ play $ drawCircle
    # pauseAtEnd recDur
  recDur <- withSceneDuration $ do
    wait 1
  return ()-}

mainScene :: Animation
mainScene = sceneAnimation $ mdo
    play $ drawCircle
      # setDuration drawCircleT
    fork $ play $ drawCircle
      # freezeAtPercentage 1
      # setDuration rotDur
    -- fork $
    --   play $ drawCircle
    --     # setDuration drawCircleT
    --     # pauseAtEnd (rotateT * (rotateN+1))
    -- wait drawCircleT
    rotDur <- withSceneDuration $ waitAll $
      forM_ svgs $ \svg -> do
        fork $ play $ drawTick
          # setDuration rotateT
          # repeatAnimation rotateN
          # applyE (overBeginning 0.5 drawInE)
          # applyE (overEnding 0.5 drawOutE)
        fork $ play $ drawSVG svg
          # setDuration rotateT
          # repeatAnimation rotateN
          # applyE (overBeginning rotateT drawInE)
          # applyE (delayE rotateT $ overBeginning 1 fillInE)
          # applyE (overEnding 0.5 fadeOutE)
        wait (rotateT / fromIntegral (1+length svgs))
    play $ drawCircle
      # setDuration drawCircleT
      # reverseAnimation
      # applyE (constE $ scaleXY (-1) 0)
  where
    drawCircleT = 1
    rotateT     = 5
    rotateN     = 3

    svg = center $ latex "\\LaTeX"
    getNth n = snd (splitGlyphs [n] svg)
    svgs = [
        scale 4 $
        translate 0 (-5) $
        withStrokeWidth (Num 0.2) $
        withStrokeColor "white" $
        withFillColor "white" $
        center $ getNth n
      | n <- [0..4]]

radius = 20
tickLength = 5

drawCircle :: Animation
drawCircle = mkAnimation 1 $ do
  n <- getSignal signalLinear
  emit $
    withFillOpacity 0 $
    withStrokeColor "white" $
    rotate 90 $
    partialSvg n circPath
  where
    circPath = pathify $ mkCircle (Num radius)

drawTick :: Animation
drawTick = mkAnimation 1 $ do
  n <- getSignal signalLinear
  emit $
    withStrokeColor "white" $
    rotate (-n*360) $
    mkLine (Num 0, Num $ -radius) (Num 0, Num $ -radius - tickLength)

drawSVG :: Tree -> Animation
drawSVG t = mkAnimation 1 $ do
  n <- getSignal signalLinear
  emit $
    withStrokeColor "white" $
    rotate (-n*360) $
    translate 0 (-radius - tickLength) $
    t
{-
  mkAnimation 5 $ do
    emit $ mkBackground "black"
    n <- getSignal $ signalLinear
    let px offset = promotePixel $ sinebow ((n+offset) `mod'` 1) :: PixelRGBA8
    emit $ withStrokeWidth (Num 0.1) $
      withStrokeColor "white" $
      withSubglyphs [0] (withFillColorPixel $ px (0/5)) $
      withSubglyphs [1] (withFillColorPixel $ px (1/5)) $
      withSubglyphs [2] (withFillColorPixel $ px (2/5)) $
      withSubglyphs [3] (withFillColorPixel $ px (3/5)) $
      withSubglyphs [4] (withFillColorPixel $ px (4/5)) $
      svg
  where
    svg = scale 10 $ center $ latex "\\LaTeX"
-}
