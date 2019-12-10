#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Codec.Picture
import qualified Data.Colour.CIE            as CIE
import           Data.Colour.CIE.Illuminant (d65)
import           Data.Colour.RGBSpace
import           Data.Colour.SRGB
import           Data.Word
import           Graphics.SvgTree           (Tree)
import           Reanimate

-- Cycle the animation if we want to upload it to youtube.
youtube :: Animation -> Animation
-- youtube = repeatAnimation 6
youtube = id

main :: IO ()
main = reanimate $ youtube $ pauseAtEnd 2 $ playThenReverseA $ pauseAtEnd 2 $ mkAnimation 5 $ \t ->
    let s           = curveS 2 t
        offsetWidth = screenWidth * 0.5
        nubWidth    = 0.2
        textYOffset = 0.2
    in mkGroup
      [ mkBackground "black"
      , translate 0 (screenHeight/2*0.85) $ withFillColor "white" $ mkGroup
        [ translate (offsetWidth*s - offsetWidth/2) 0 $
          withFillColor "white" $ mkCircle nubWidth
        , withStrokeColor "white" $ withStrokeWidth 0.05 $
          mkLine (-(offsetWidth-nubWidth)/2, 0)
                 ((offsetWidth-nubWidth)/2, 0)
        , translate (-offsetWidth/2-1.0) textYOffset $
          scale 0.5 $ centerX $ latex "Color"
        , translate (offsetWidth/2+1.5) textYOffset $
          scale 0.5 $ centerX $ latex "Greyscale"
        ]
      , translate (-columnX) (rowInit-rowStep*0) $ mkOutline "viridis" (dimmer s . viridis)
      , translate (-columnX) (rowInit-rowStep*1) $ mkOutline "inferno" (dimmer s . inferno)
      , translate (-columnX) (rowInit-rowStep*2) $ mkOutline "cividis" (dimmer s . cividis)
      , translate (-columnX) (rowInit-rowStep*3) $ mkOutline "jet" (dimmer s . jet)
      , translate (-columnX) (rowInit-rowStep*4) $ mkOutline "turbo" (dimmer s . turbo)

      , translate columnX (rowInit-rowStep*0) $ mkOutline "magma" (dimmer s . magma)
      , translate columnX (rowInit-rowStep*1) $ mkOutline "plasma" (dimmer s . plasma)
      , translate columnX (rowInit-rowStep*2) $ mkOutline "sinebow" (dimmer s . sinebow)
      , translate columnX (rowInit-rowStep*3) $ mkOutline "hsv" (dimmer s . hsv)
      , translate columnX (rowInit-rowStep*4) $ mkOutline "parula" (dimmer s . parula)
      ]
  where
    rowInit = 2.2
    rowStep = 1.2
    columnX = screenWidth/4

    mkOutline label f =
      mkGroup
      [ center $ withFillColor "grey" $ mkRect (scaleWidth+0.05) (scaleHeight+0.05)
      , scaleToSize scaleWidth scaleHeight $ mkColorMap f
      , translate (-scaleWidth/2) 0.5 $ centerY $ withFillColor "white" $
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
