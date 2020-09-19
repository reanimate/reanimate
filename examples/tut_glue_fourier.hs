#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Codec.Picture
import           Control.Lens
import           Data.Complex
import           Graphics.SvgTree
import           Linear.V2
import           Reanimate

-- layer 3
main :: IO ()
main = reanimate $ setDuration 30 $ scene $ do
    _ <- newSpriteSVG $ mkBackgroundPixel (PixelRGBA8 252 252 252 0xFF)
    play $ fourierA (fromToS 0 5)      -- Rotate 15 times
      & setDuration 50
      & signalA (reverseS . powerS 2 . reverseS) -- Start fast, end slow
      & pauseAtEnd 2
    play $ fourierA (constantS 0)       -- Don't rotate at all
      & setDuration 10
      & reverseA
      & signalA (powerS 2)                       -- Start slow, end fast
      & pauseAtEnd 2

-- layer 2
fourierA :: (Double -> Double) -> Animation
fourierA genPhi = animate $ \t ->
    let circles = setFourierLength (t*piFourierLen) piFourier
        coeffs = fourierCoefficients $ rotateFourier (genPhi t) circles
    in mkGroup
    [ drawCircles coeffs
    , withStrokeColor "green" $
      withStrokeLineJoin JoinRound $
      withFillOpacity 0 $
      withStrokeWidth (defaultStrokeWidth*2) $
      mkLinePath $ mkFourierOutline circles
    , let x :+ y = sum coeffs in
      translate x y $ withFillColor "red" $ mkCircle (defaultStrokeWidth*3)
    ]

drawCircles :: [Complex Double] -> SVG
drawCircles [] = mkGroup []
drawCircles ( x :+ y : xs) =
  translate x y $ drawCircles' xs

drawCircles' :: [Complex Double] -> SVG
drawCircles' circles = mkGroup
    [ worker circles
    , withStrokeColor "black" $
      withStrokeLineJoin JoinRound $
      withFillOpacity 0 $
      mkLinePath [ (x, y) | x :+ y <- scanl (+) 0 circles ]]
  where
    worker [] = None
    worker (x :+ y : rest) =
      let radius = sqrt(x*x+y*y) in
      mkGroup
      [ withStrokeColor "dimgrey" $
        withFillOpacity 0 $
        mkCircle radius
      , translate x y $ worker rest ]

-- layer 1
newtype Fourier = Fourier {fourierCoefficients :: [Complex Double]}

piFourier :: Fourier
piFourier = mkFourier $ lineToPoints 500 $
  toLineCommands $ extractPath $ scale 15 $
  center $ latexAlign "\\pi"

piFourierLen :: Double
piFourierLen = sum $ map magnitude $ drop 1 $ take 500 $ fourierCoefficients piFourier

pointAtFourier :: Fourier -> Complex Double
pointAtFourier = sum . fourierCoefficients

mkFourier :: [RPoint] -> Fourier
mkFourier points = Fourier $ findCoefficient 0 :
    concat [ [findCoefficient n, findCoefficient (-n)] | n <- [1..] ]
  where
    findCoefficient :: Int -> Complex Double
    findCoefficient n =
        sum [ toComplex point * exp (negate (fromIntegral n) * 2 *pi * i*t) * deltaT
            | (idx, point) <- zip [0::Int ..] points, let t = fromIntegral idx/nPoints ]
    i = 0 :+ 1
    toComplex (V2 x y) = x :+ y
    deltaT = recip nPoints
    nPoints = fromIntegral (length points)

setFourierLength :: Double -> Fourier -> Fourier
setFourierLength _ (Fourier []) = Fourier []
setFourierLength len0 (Fourier (first:lst)) = Fourier $ first : worker len0 lst
  where
    worker _len [] = []
    worker len (c:cs) =
      if magnitude c < len
        then c : worker (len - magnitude c) cs
        else [c * realToFrac (len / magnitude c)]

rotateFourier :: Double -> Fourier -> Fourier
rotateFourier phi (Fourier coeffs) =
    Fourier $ worker coeffs (0::Integer)
  where
    worker [] _ = []
    worker (x:rest) 0 = x : worker rest 1
    worker [left] n = worker [left,0] n
    worker (left:right:rest) n =
      let n' = fromIntegral n in
      left * exp (negate n' * 2 * pi * i * phi') :
      right * exp (n' * 2 * pi * i * phi') :
      worker rest (n+1)
    i = 0 :+ 1
    phi' = realToFrac phi

mkFourierOutline :: Fourier -> [(Double, Double)]
mkFourierOutline fourier =
    [ (x, y)
    | idx <- [0 .. granularity]
    , let x :+ y = pointAtFourier $ rotateFourier (idx/granularity) fourier
    ]
  where
    granularity = 500
