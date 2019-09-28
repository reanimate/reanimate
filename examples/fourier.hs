#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Data.Complex
import qualified Data.Text as T

import           Graphics.SvgTree
import           Reanimate.Driver (reanimate)
import           Reanimate.LaTeX  (latex)
import           Reanimate.Animation
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

sWidth :: Double
sWidth = 0.02

fourierAnimation :: Int -> Animation
fourierAnimation nCircles = repeatA 2 $ mkAnimation 3 $ \t ->
    let phi = signalFromTo 0 (2*pi) signalLinear t
    in mkGroup
    [ mkBackground "black"
    , translate (-screenWidth/4) 0 $ mkGroup
      [ drawNCircles nCircles phi
      , withStrokeColor "white" $
        withStrokeWidth sWidth $
        withFillOpacity 0 $
        translate (screenWidth/4) 0 $
        mkCirclePath nCircles phi ]
    , withStrokeWidth sWidth $
      withFillColor "white" $
      translate (-screenWidth/8*3) (screenHeight/8*3) $
      latex $ T.pack $ "Circles: " ++ show nCircles ]

drawNCircles :: Int -> Double -> Tree
drawNCircles totalCircles phi = mkGroup
    [ worker circles
    , let x :+ y = sum circles in
      withStrokeWidth sWidth $
      withStrokeColor "white" $
      mkLine (x, y) (screenWidth/4, y) ]
  where
    circles = [ nthCircle n phi | n <- [0..totalCircles-1] ]
    worker [] = None
    worker (x :+ y : rest) =
      let radius = sqrt(x*x+y*y) in
      mkGroup
      [ withStrokeWidth sWidth $
        withStrokeColor "grey" $
        withFillOpacity 0 $
        mkCircle radius
      , translate x y $ worker rest
      , withStrokeWidth sWidth $
        withStrokeColor "white" $
        mkLine (0, 0) (x, y) ]

mkCirclePath :: Int -> Double -> Tree
mkCirclePath nCircles phiOffset = mkLinePath $ take 2000 $
    zip [ 2 * i/granularity | i <- [0..]]
    $ drop (round $ (1-phiOffset/(2*pi)) * granularity) $
    cycle $ [ fourierYValue nCircles phi
    | x <- reverse [1..granularity]
    , let phi = 2*pi*(x/granularity)
    ]
  where
    granularity = 500

fourierYValue :: Int -> Double -> Double
fourierYValue n phi =
  imagPart (sum [ nthCircle i phi | i <- [0..n-1]])

nthCircle :: Int -> Double -> Complex Double
nthCircle n phi = x :+ y
  where
    n' = fromIntegral (n*waveMultiplier+1)
    x = cos (n'*phi) * radius
    y = sin (n'*phi) * radius
    radius = 2.5 * (2 / (n'*pi))
