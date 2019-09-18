#!/usr/bin/env stack
-- stack runghc --package reanimate
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

screenWidth = 16
screenHeight = 9

main :: IO ()
main = reanimate $ youtube $ pauseAtEnd 2 $ autoReverse $ pauseAtEnd 2 $ mkAnimation 5 $ do
    s <- getSignal $ signalCurve 2
    let scaleWidth = screenWidth * 0.5
        nubWidth = 0.2
        textYOffset = 0.2
    emit $ mkGroup
      [ mkBackground "black"
      , translate 0 (screenHeight/2*0.85) $ withFillColor "white" $ mkGroup
        [ translate (scaleWidth*s - scaleWidth/2) 0 $
          withFillColor "white" $ mkCircle (Num nubWidth)
        , withStrokeColor "white" $ withStrokeWidth (Num 0.05) $
          mkLine (Num $ -(scaleWidth-nubWidth)/2,Num 0)
                 (Num $ (scaleWidth-nubWidth)/2, Num 0)
        , translate (-scaleWidth/2-1.0) textYOffset $
          scale 0.5 $ centerX $ latex "Color"
        , translate (scaleWidth/2+1.5) textYOffset $
          scale 0.5 $ centerX $ latex "Greyscale"
        ]
      , translate (-columnX) (rowInit-rowStep*0) $ mkOutline "viridis" (dimmer s . viridis)
      , translate (-columnX) (rowInit-rowStep*1) $ mkOutline "inferno" (dimmer s . inferno)
      , translate (-columnX) (rowInit-rowStep*2) $ mkOutline "cividis" (dimmer s . cividis)
      , translate (-columnX) (rowInit-rowStep*3) $ mkOutline "jet" (dimmer s . jet)
      , translate (-columnX) (rowInit-rowStep*4) $ mkOutline "turbo" (dimmer s . turbo)

      , translate (columnX) (rowInit-rowStep*0) $ mkOutline "magma" (dimmer s . magma)
      , translate (columnX) (rowInit-rowStep*1) $ mkOutline "plasma" (dimmer s . plasma)
      , translate (columnX) (rowInit-rowStep*2) $ mkOutline "sinebow" (dimmer s . sinebow)
      , translate (columnX) (rowInit-rowStep*3) $ mkOutline "hsv" (dimmer s . hsv)
      , translate (columnX) (rowInit-rowStep*4) $ mkOutline "parula" (dimmer s . parula)
      ]
  where
    rowInit = 2.2
    rowStep = 1.2
    columnX = screenWidth/4

    mkOutline label f =
      mkGroup
      [ center $ withFillColor "grey" $ mkRect (Num $ scaleWidth+0.05) (Num $ scaleHeight+0.05)
      , scaleToSize scaleWidth scaleHeight $ mkColorMap f
      , translate (-scaleWidth/2) (0.5) $ centerY $ withFillColor "white" $
        scale 0.5 $ latex label
      ]
    scaleWidth = screenWidth/8*3
    scaleHeight = screenHeight/20

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
