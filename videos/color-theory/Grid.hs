{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ApplicativeDo     #-}
module Grid
  ( gridScene
  )
where

import qualified Data.ByteString               as BS
import qualified Data.Text                     as T
import           Codec.Picture
import           Data.Maybe
import           Reanimate
import           Reanimate.Scene
import           Transcript
import           Common
import           System.IO.Unsafe

gridScene :: Scene s ()
gridScene = spriteScope $ do
  sViridis <- monalisaSprite (-1) 1 "viridis"
  sCividis <- monalisaSprite 0 1 "cividis"
  sParula  <- monalisaSprite 1 1 "parula"

  sJet     <- monalisaSprite (-1) 0 "jet"
  sInferno <- monalisaSprite 0 0 "inferno"
  sSinebow <- monalisaSprite 1 0 "sinebow"

  sTurbo   <- monalisaSprite (-1) (-1) "turbo"
  sPlasma  <- monalisaSprite 0 (-1) "plasma"
  sHSV     <- monalisaSprite 1 (-1) "hsv"

  waitUntil $ wordStart $ findWord ["grid"] "Turbo"
  showMap sTurbo "distortions"

  waitUntil $ wordEnd $ findWord ["grid"] "And"
  showMap sCividis "form"

  waitUntil $ wordStart $ findWord ["end"] "The"

  return ()
 where
  showMap hd wd = do
    spriteZ (fst hd) 1
    tweenVar (snd hd) 1 $ \v -> fromToS v 1 . curveS 3
    waitUntil $ wordEnd (findWord ["grid"] wd)
    wait (-1)
    tweenVar (snd hd) 1 $ \v -> fromToS v 0 . curveS 3
    spriteZ (fst hd) 0

maps =
  [ ("viridis", viridis)
  , ("cividis", cividis)
  , ("parula" , parula)
  , ("jet"    , jet)
  , ("inferno", inferno)
  , ("sinebow", sinebow)
  , ("turbo"  , turbo)
  , ("plasma" , plasma)
  , ("hsv"    , hsv)
  ]

monalisaSprite :: Double -> Double -> T.Text -> Scene s (Sprite s, Var s Double)
monalisaSprite x y txt = do
  highlight <- newVar 0
  s         <- newSprite $ do
    getHighlight <- unVar highlight
    return
      $ translate (screenWidth / 3 * x * (1 - getHighlight))
                  (screenHeight / 3 * y * (1 - getHighlight))
      $ scale (fromToS 1 2 getHighlight)
      $ mkGroup
          [ scaleToSize (screenWidth / 3) (screenHeight / 3)
          $ embedImage
          $ applyColorMap (fromMaybe jet $ lookup txt maps) monalisa
          , translate (-screenWidth / 6 + screenWidth * 0.005)
                      (screenHeight / 6 - 0.35)
          $ scale 0.5
          $ withStrokeColor "black"
          $ withStrokeWidth (defaultStrokeWidth*3)
          $ withFillColor "white"
          $ latex ("\\texttt{" <> txt <> "}")
          ]
  return (s, highlight)

monalisaPoster :: SVG
monalisaPoster = mkGroup
  [ mkPic (-1) 1    viridis "viridis"
  , mkPic 0    1    cividis "cividis"
  , mkPic 1    1    parula  "parula"
  , mkPic (-1) 0    jet     "jet"
  , mkPic 0    0    inferno "inferno"
  , mkPic 1    0    sinebow "sinebow"
  , mkPic (-1) (-1) turbo   "turbo"
  , mkPic 0    (-1) plasma  "plasma"
  , mkPic 1    (-1) hsv     "hsv"
  ]
 where
  mkPic x y cm txt =
    translate (screenWidth / 3 * x) (screenHeight / 3 * y) $ mkGroup
      [ scaleToSize (screenWidth / 3) (screenHeight / 3)
      $ embedImage
      $ applyColorMap cm monalisa
      , translate (-screenWidth / 6) (screenHeight / 6)
      $ scale 0.5
      $ withStrokeColor "black"
      $ withStrokeWidth (defaultStrokeWidth * 0.5)
      $ withFillColor "white"
      $ latex ("\\texttt{" <> txt <> "}")
      ]
