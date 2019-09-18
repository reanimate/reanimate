#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Lens
import           Data.Complex
import qualified Data.Text as T

import           Graphics.SvgTree
import           Reanimate.Driver (reanimate)
import           Reanimate.LaTeX  (latex)
import           Reanimate.Monad
import           Reanimate.Svg
import           Reanimate.Signal
import           Reanimate.Constants

waveMultiplier :: Int
-- waveMultiplier = 1 -- Sawtooth wave
waveMultiplier = 2 -- Square wave

main :: IO ()
main = reanimate $
  fourierAnimation 1 `before`
  fourierAnimation 2 `before`
  fourierAnimation 3 `before`
  fourierAnimation 5 `before`
  fourierAnimation 10 `before`
  fourierAnimation 25 `before`
  fourierAnimation 50 `before`
  fourierAnimation 100

sWidth = 0.02

fourierAnimation :: Int -> Animation
fourierAnimation nCircles = repeatAnimation 2 $ mkAnimation 3 $ do
    emit $ mkBackground "black"
    phi <- getSignal $ signalFromTo 0 (2*pi) signalLinear
    mapF (translate (-screenWidth/4) 0) $ do
      drawNCircles nCircles phi
      emit $ withStrokeColor "white" $
        withStrokeWidth (Num sWidth) $
        withFillOpacity 0 $
        translate (screenWidth/4) 0 $
        mkCirclePath nCircles phi
    emit $ withStrokeWidth (Num sWidth) $
      withFillColor "white" $
      translate (-screenWidth/8*3) (screenHeight/8*3) $
      latex $ T.pack $ "Circles: " ++ show nCircles

drawNCircles totalCircles phi = do
    worker circles
    let x :+ y = sum circles
    emit $ withStrokeWidth (Num sWidth) $
      withStrokeColor "white" $
      mkLine (Num x, Num y) (Num (screenWidth/4), Num y)
  where
    circles = [ nthCircle n phi | n <- [0..totalCircles-1] ]
    worker [] = return ()
    worker (x :+ y : rest) = do
      let radius = sqrt(x*x+y*y)
      emit $ withStrokeWidth (Num sWidth) $
        withStrokeColor "grey" $
        withFillOpacity 0 $
        CircleTree $ defaultSvg
          & circleCenter .~ (Num 0, Num 0)
          & circleRadius .~ Num radius
      mapF (translate x y) $ worker rest
      emit $ withStrokeWidth (Num sWidth) $
        withStrokeColor "white" $
        mkLine (Num 0, Num 0) (Num x, Num y)

mkCirclePath nCircles phiOffset = mkLinePath $ take 2000 $
    zip [ 2 * i/granularity | i <- [0..]]
    $ drop (round $ (1-phiOffset/(2*pi)) * granularity) $
    cycle $ [ fourierYValue nCircles phi
    | x <- reverse [1..granularity]
    , let phi = 2*pi*(x/granularity)
    ]
  where
    granularity = 500

fourierYValue n phi =
  imagPart (sum [ nthCircle i phi | i <- [0..n-1]])

nthCircle :: Int -> Double -> Complex Double
nthCircle n phi = x :+ y
  where
    n' = fromIntegral (n*waveMultiplier+1)
    x = cos (n'*phi) * radius
    y = sin (n'*phi) * radius
    radius = 2.5 * (2 / (n'*pi))
