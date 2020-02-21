#!/usr/bin/env stack
-- stack --resolver lts-13.14 runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ApplicativeDo     #-}
module Grid (gridScene) where

import           Control.Lens          ()
import           Control.Monad
import qualified Data.ByteString       as BS
import qualified Data.Map              as Map
import           Data.Monoid
import qualified Data.Text             as T

import           Codec.Picture
import           Codec.Picture.Jpg
import           Codec.Picture.Types
import           Data.Maybe
import           Data.Word
import           Graphics.SvgTree      hiding (Image, imageHeight, imageWidth)
import           Graphics.SvgTree.Memo
import           Numeric
import           Reanimate
import           Reanimate.Animation
import           Reanimate.ColorMap
import           Reanimate.ColorSpace
import           Reanimate.Constants
import           Reanimate.Driver      (reanimate)
import           Reanimate.Effect
import           Reanimate.LaTeX
import           Reanimate.Raster
import           Reanimate.Scene
import           Reanimate.Signal
import           Reanimate.Svg
import           System.IO.Unsafe

gridScene :: Animation
gridScene = sceneAnimation $ do
  sViridis <- monalisaSprite (-1) 1 "viridis"
  sCividis <- monalisaSprite 0 1 "cividis"
  sParula <- monalisaSprite 1 1 "parula"

  sJet <- monalisaSprite (-1) 0 "jet"
  sInferno <- monalisaSprite 0 0 "inferno"
  sSinebow <- monalisaSprite 1 0 "sinebow"

  sTurbo <- monalisaSprite (-1) (-1) "turbo"
  sPlasma <- monalisaSprite 0 (-1) "plasma"
  sHSV <- monalisaSprite 1 (-1) "hsv"

  wait 3

  showMap sViridis
  showMap sCividis
  showMap sParula
  wait 2

  showMap sJet
  showMap sInferno
  showMap sSinebow
  wait 2

  showMap sTurbo
  showMap sPlasma
  showMap sHSV
  wait 2

  return ()
  where
    showMap hd = do
      spriteZ (fst hd) 1
      tweenVar (snd hd) 1 $ \v -> fromToS v 1 . curveS 3
      wait 1
      tweenVar (snd hd) 1 $ \v -> fromToS v 0 . curveS 3
      spriteZ (fst hd) 0

maps =
  [ ("viridis", viridis)
  , ("cividis", cividis)
  , ("parula", parula)
  , ("jet", jet)
  , ("inferno", inferno)
  , ("sinebow", sinebow)
  , ("turbo", turbo)
  , ("plasma", plasma)
  , ("hsv", hsv)
  ]

monalisaSprite :: Double -> Double -> T.Text -> Scene s (Sprite s, Var s Double)
monalisaSprite x y txt = do
  highlight <- newVar 0
  s <- newSprite $ do
    getHighlight <- unVar highlight
    return $
      translate (screenWidth/3 * x * (1-getHighlight))
                (screenHeight/3 * y * (1-getHighlight)) $
      scale (fromToS 1 2 (getHighlight)) $
      mkGroup
      [ scaleToSize (screenWidth/3) (screenHeight/3) $
        embedImage $
        applyColorMap (fromMaybe jet $ lookup txt maps) monalisa
      , translate (-screenWidth/6 + screenWidth*0.005) (screenHeight/6 - screenHeight*0.005) $
        scale 0.5 $
        withStrokeColor "black" $
        withStrokeWidth (defaultStrokeWidth*0.5) $
        withFillColor "white" $
        latex ("\\texttt{" <> txt <> "}")
      ]
  return (s, highlight)

monalisaPoster :: SVG
monalisaPoster =
    mkGroup
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
