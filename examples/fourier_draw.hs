#!/usr/bin/env stack
-- stack --resolver lts-12.26 runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Lens
import           Control.Monad
import           Data.Complex
import qualified Data.Text        as T

import           Graphics.SvgTree
import           Linear.V2
import           Reanimate.Driver (reanimate)
import           Reanimate.LaTeX
import           Reanimate.Monad
import           Reanimate.Svg

waveMultiplier :: Int
-- waveMultiplier = 1 -- Sawtooth wave
waveMultiplier = 2 -- Square wave

main :: IO ()
main = reanimate $
  fourierAnimation 0 `before`
  fourierAnimation 1 `before`
  fourierAnimation 2 `before`
  fourierAnimation 3 `before`
  fourierAnimation 4 `before`
  fourierAnimation 5 `before`
  fourierAnimation 6 `before`
  fourierAnimation 7 `before`
  fourierAnimation 8 `before`
  fourierAnimation 9 `before`
  fourierAnimation 10 `before`
  fourierAnimation 15 `before`
  fourierAnimation 25 `before`
  fourierAnimation 40

sWidth = 0.5

fourierAnimation :: Int -> Animation
fourierAnimation nCircles = repeatAnimation 1 $ mkAnimation 10 $ do
    emit $ mkBackground "black"
    phi <- signal 0 1

    let circles = findCoefficients nCircles
    emit $ withStrokeWidth (Num 1) $ withStrokeColor "green" $
      mkCirclePath circles
    drawNCircles $ rearrangeCircles $ rotateCircles phi circles

    emit $ withStrokeWidth (Num sWidth) $
      withFillColor "white" $
      translate (-140) (-80) $
      scale 2 $ latex $ T.pack $ "Circles: " ++ show (nCircles*2+1)

piPoints :: [RPoint]
piPoints = lineToPoints 500 $
  toLineCommands $ extractPath $ scale 30 $ center $ latexAlign "\\pi"

findCoefficient :: Int -> Complex Double
findCoefficient n =
    sum [ toComplex point * exp (negate (fromIntegral n) * 2 *pi * i*t) * deltaT
        | (idx, point) <- zip [0..] piPoints, let t = fromIntegral idx/nPoints ]
  where
    i = 0 :+ 1
    toComplex (V2 x y) = x :+ y
    deltaT = recip nPoints
    nPoints = fromIntegral (length piPoints)

findCoefficients :: Int -> [Complex Double]
findCoefficients n = [ findCoefficient idx | idx <- [-n .. n] ]

rotateCircles :: Double -> [Complex Double] -> [Complex Double]
rotateCircles phi coeffs =
    [ coeff * exp (fromIntegral idx *2*pi*i*realToFrac phi)| (coeff, idx) <- zip coeffs [-n..] ]
  where
    i = 0 :+ 1
    n = length coeffs `div` 2

rearrangeCircles :: [Complex Double] -> [Complex Double]
rearrangeCircles [coeff] = [coeff]
rearrangeCircles coeffs =
    coeffs !! n : zipList (reverse (take n coeffs)) (drop (n+1) coeffs)
  where
    zipList (x:xs) (y:ys) = x : y : zipList xs ys
    zipList lst [] = lst
    zipList [] lst = lst
    n = length coeffs `div` 2

drawNCircles :: [Complex Double] -> Frame ()
drawNCircles circles = do
    worker circles
    emit $ withStrokeWidth (Num sWidth) $
      withStrokeColor "white" $
      withFillOpacity 0 $
      mkLinePath [ (x, y) | x :+ y <- scanl (+) 0 circles ]
  where
    worker [] = return ()
    worker (x :+ y : rest) = do
      let radius = sqrt(x*x+y*y)
      emit $ withStrokeWidth (Num 0.2) $
        withStrokeColor "dimgrey" $
        withFillOpacity 0 $
        CircleTree $ defaultSvg
          & circleCenter .~ (Num 0, Num 0)
          & circleRadius .~ Num radius
      mapF (translate x y) $ worker rest

mkCirclePath circles = mkLinePath
    [ (x, y)
    | idx <- [0 .. granularity]
    , let t = idx/granularity
          x :+ y = sum (rotateCircles t circles)
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
    radius = 40 * (2 / (n'*pi))
