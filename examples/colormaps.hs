#!/usr/bin/env stack
-- stack --resolver lts-13.14 runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Lens               ()

import           Codec.Picture
import qualified Data.Colour.CIE            as CIE
import           Data.Colour.CIE.Illuminant
import           Data.Colour.RGBSpace
import qualified Data.Colour.RGBSpace.HSL   as HSL
import           Data.Colour.SRGB
import           Data.Word
import           Graphics.SvgTree           (Number (..), Tree)
import           Reanimate.ColorMap
import           Reanimate.Driver           (reanimate)
import           Reanimate.LaTeX
import           Reanimate.Monad
import           Reanimate.Raster
import           Reanimate.Signal
import           Reanimate.Svg

-- Cycle the animation if we want to upload it to youtube.
youtube :: Animation -> Animation
-- youtube = repeatAnimation 6
youtube = id

main :: IO ()
main = reanimate $ youtube $ pauseAtEnd 2 $ autoReverse $ pauseAtEnd 2 $ mkAnimation 5 $ do
    s <- getSignal $ signalCurve 2
    let scaleWidth = 50
        nubWidth = 2
    emit $ mkGroup
      [ mkBackground "black"
      , translate 0 (-80) $ withFillColor "white" $ mkGroup
        [ translate (scaleWidth*s - scaleWidth/2) 0 $
          withFillColor "white" $ mkCircle (Num 0, Num 0) (Num nubWidth)
        , withStrokeColor "white" $ withStrokeWidth (Num 0.5) $
          mkLine (Num $ -(scaleWidth-nubWidth)/2,Num 0) (Num $ (scaleWidth-nubWidth)/2, Num 0)
        , translate (-scaleWidth/2-20) (-5) $ centerX $ latex "Color"
        , translate (scaleWidth/2+27) (-5) $ centerX $ latex "Greyscale"
        ]
      , translate (-80) (-50) $ mkOutline "viridis" (dimmer s . viridis)
      , translate (-80) (-20) $ mkOutline "inferno" (dimmer s . inferno)
      , translate (-80) (10) $ mkOutline "cividis" (dimmer s . cividis)
      , translate (-80) (40) $ mkOutline "jet" (dimmer s . jet)
      , translate (-80) (70) $ mkOutline "turbo" (dimmer s . turbo)

      , translate (80) (-50) $ mkOutline "magma" (dimmer s . magma)
      , translate (80) (-20) $ mkOutline "plasma" (dimmer s . plasma)
      , translate (80) (10) $ mkOutline "sinebow" (dimmer s . sinebow)
      , translate (80) (40) $ mkOutline "hsv" (dimmer s . hsv)
      , translate (80) (70) $ mkOutline "parula" (dimmer s . parula)
      ]
  where
    mkOutline label f =
      mkGroup
      [ center $ withFillColor "grey" $ mkRect (Num 0, Num 0) (Num 151) (Num 11)
      , scaleToSize 150 10 $ mkColorMap f
      , translate (-75) (-10) $ centerY $ withFillColor "white" $
        latex label
      ]

mkColorMap :: (Double -> PixelRGB8) -> Tree
mkColorMap f = center $ embedImage img
  where
    width = 1000
    height = 1
    img = generateImage pixelRenderer width height
    pixelRenderer x _y = f (fromIntegral x / fromIntegral width)

dimmer :: Double -> PixelRGB8 -> PixelRGB8
dimmer switch (PixelRGB8 r g b) =
    PixelRGB8 r' g' b'
  where
    (lStar, aStar, bStar) = CIE.cieLABView d65 (sRGBBounded r g b :: Colour Double)
    RGB r' g' b' = toSRGBBounded $ CIE.cieLAB d65 lStar (aStar*c) (bStar*c) :: RGB Word8
    c = 1-switch
