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
import           Reanimate.Combinators

waveMultiplier :: Int
-- waveMultiplier = 1 -- Sawtooth wave
waveMultiplier = 2 -- Square wave

main :: IO ()
main = reanimate $ pauseAtEnd 2 $
  -- fourierAnimation 1 2
  fourierAnimation 1 2 `before`
  fourierAnimation 2 3 `before`
  fourierAnimation 3 4 `before`
  fourierAnimation 4 5 `before`
  fourierAnimation 5 6 `before`
  fourierAnimation 6 7 `before`
  fourierAnimation 7 8 `before`
  fourierAnimation 8 9 `before`
  fourierAnimation 9 10 `before`
  fourierAnimation 10 15 `before`
  fourierAnimation 15 25 `before`
  fourierAnimation 25 40 `before`
  fourierAnimation 40 40

sWidth = 0.5

applyMorph :: Int -> [Complex Double] -> Complex Double -> [Complex Double]
applyMorph n coeffs mult =
  map (*mult) (take n coeffs) ++
  take (length coeffs - n * 2) (drop n coeffs) ++
  map (*mult) (reverse $ take n $ reverse coeffs)

fourierAnimation :: Int -> Int -> Animation
fourierAnimation nCircles nextCircles = repeatAnimation 1 $ mkAnimation 10 $ do
    emit $ mkBackground "black"
    phi <- signal 0 1

    let circles = applyMorph (nextCircles - nCircles) (findCoefficients nextCircles) circleAlpha
        circleAlpha = realToFrac (max 0 (phi-0.80) / 0.20)

    emit $ withStrokeWidth (Num 1) $ withStrokeColor "green" $
      mkLinePath $ mkCirclePath circles
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
      withStrokeLineJoin JoinRound $
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

mkCirclePath circles =
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
