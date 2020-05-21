#!/usr/bin/env stack
-- stack --resolver lts-13.14 runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main (main) where

import           Control.Lens          ()
import           Control.Monad
import qualified Data.ByteString       as BS
import qualified Data.Map              as Map
import           Data.Monoid
import qualified Data.Text             as T

import           Codec.Picture
import           Codec.Picture.Jpg
import           Codec.Picture.Types
import           Data.Word
import           Graphics.SvgTree      hiding (Image, imageHeight, imageWidth)
import           Graphics.SvgTree.Memo
import           Numeric
import           Reanimate.ColorMap
import           Reanimate.Driver      (reanimate)
import           Reanimate.LaTeX
import           Reanimate
import           Reanimate.Animation
import           Reanimate.Raster
import           Reanimate.Scene
import           Reanimate.Ease
import           Reanimate.Effect
import           Reanimate.Svg
import           Reanimate.ColorSpace
import           Reanimate.Constants
import           System.IO.Unsafe

main :: IO ()
main = reanimate $
  mkAnimation (1/60) (const monalisaPoster)

monalisaPoster :: SVG
monalisaPoster =
    mkGroup
    --[ mkPic (-1) 1 jet,       mkPic 0 1 turbo,      mkPic 1 1 parula
    --, mkPic (-1) 0 viridis,   mkPic 0 0 inferno,    mkPic 1 0 sinebow
    --, mkPic (-1) (-1) plasma, mkPic 0 (-1) cividis, mkPic 1 (-1) hsv ]

    [ mkPic (-1) 1 viridis "viridis",   mkPic 0 1 cividis "cividis",    mkPic 1 1 parula "parula"
    , mkPic (-1) 0 jet "jet",   mkPic 0 0 inferno "inferno",    mkPic 1 0 sinebow "sinebow"
    , mkPic (-1) (-1) turbo "turbo", mkPic 0 (-1) plasma "plasma", mkPic 1 (-1) hsv "hsv" ]
  where
    mkPic x y cm txt =
      translate (screenWidth/3 * x) (screenHeight/3 * y) $
      mkGroup
      [ scaleToSize (screenWidth/3) (screenHeight/3) $ embedImage $
        applyColorMap cm monalisa
      , translate (-screenWidth/6) (screenHeight/6) $
        scale 0.5 $
        withStrokeColor "black" $
        withStrokeWidth (defaultStrokeWidth*0.5) $
        withFillColor "white" $
        latex ("\\texttt{" <> txt <> "}")
      ]

monalisa :: Image PixelRGB8
monalisa = unsafePerformIO $ do
  dat <- BS.readFile "monalisa.jpg"
  case decodeJpeg dat of
    Left err  -> error err
    Right img -> return $ convertRGB8 img

monalisaLarge :: Image PixelRGB8
monalisaLarge = scaleImage 15 monalisa

scaleImage :: Pixel a => Int -> Image a -> Image a
scaleImage factor img =
    generateImage fn (imageWidth img * factor) (imageHeight img * factor)
  where
    fn x y = pixelAt img (x `div` factor) (y `div` factor)

applyColorMap :: (Double -> PixelRGB8) -> Image PixelRGB8 -> Image PixelRGB8
applyColorMap cmap img =
    generateImage fn (imageWidth img) (imageHeight img)
  where
    fn x y =
      case pixelAt img x y of
        PixelRGB8 r _ _ -> cmap (fromIntegral r/255)
