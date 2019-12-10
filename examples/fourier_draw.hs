#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Data.Complex
import qualified Data.Text           as T
import           Graphics.SvgTree
import           Linear.V2
import           Reanimate

main :: IO ()
main = reanimate $ pauseAtEnd 2
  fourierAnimation_

sWidth :: Double
sWidth = 0.02

piFourier :: Fourier
piFourier = mkFourier piPoints

piPoints :: [RPoint]
piPoints = lineToPoints 500 $
  toLineCommands $ extractPath $ scale 10 $ center $ latexAlign "\\pi"


fourierAnimation_ :: Animation
fourierAnimation_ = mkAnimation 50 $ \t ->
    let fLength = t
        circles = setFourierLength (fLength*maxLength) piFourier
        maxLength = sum $ map magnitude $ take 499 $ drop 1 $ fourierCoefficients piFourier
        phi = fromToS 0 15 t
    in mkGroup
    [ mkBackground "black"
    , drawCircles $ fourierCoefficients $ rotateFourier phi circles
    , withStrokeColor "green" $
      withFillOpacity 0 $
      mkLinePath $ mkFourierOutline circles
    , withFillColor "white" $
      translate (-screenWidth/16*7) (screenHeight/16*7) $
      latex $ T.pack $ "Circles: " ++ show (length $ fourierCoefficients circles)
    ]

newtype Fourier = Fourier {fourierCoefficients :: [Complex Double]}

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


-- setFourierCircles :: Double -> Fourier -> Fourier
-- setFourierCircles n _ | n < 1 = error "Invalid argument. Need at least one circle."
-- setFourierCircles n (Fourier coeffs) =
--     Fourier $ take iCircles coeffs ++ [coeffs!!iCircles * realToFrac fCircle]
--   where
--     (iCircles, fCircle) = divMod' n 1

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
    -- n = length coeffs `div` 2
    phi' = realToFrac phi

drawCircles :: [Complex Double] -> SVG
drawCircles circles = mkGroup
    [ worker circles
    , withStrokeWidth sWidth $
      withStrokeColor "white" $
      withStrokeLineJoin JoinRound $
      withFillOpacity 0 $
      mkLinePath [ (x, y) | x :+ y <- scanl (+) 0 circles ] ]
  where
    worker [] = None
    worker (x :+ y : rest) =
      let radius = sqrt(x*x+y*y) in
      mkGroup
      [ withStrokeWidth sWidth $
        withStrokeColor "dimgrey" $
        withFillOpacity 0 $
        mkCircle radius
      , translate x y $ worker rest ]

mkFourierOutline :: Fourier -> [(Double, Double)]
mkFourierOutline fourier =
    [ (x, y)
    | idx <- [0 .. granularity]
    , let x :+ y = pointAtFourier $ rotateFourier (idx/granularity) fourier
    ]
  where
    granularity = 500
