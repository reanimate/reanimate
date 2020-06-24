{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo     #-}
module Grid
  ( gridScene
  )
where

import qualified Data.Text                     as T
import           Data.Maybe
import           Reanimate
import           Reanimate.Scene
import           Transcript
import           Common

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
 where
  showMap (sprite, hl) wd = do
    spriteZ sprite 1
    tweenVar hl 1 $ \v -> fromToS v 1 . curveS 3
    waitUntil $ wordEnd (findWord ["grid"] wd) - 1
    tweenVar hl 1 $ \v -> fromToS v 0 . curveS 3
    spriteZ sprite 0

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
  s         <- newSpriteSVG $ mkGroup
    [ scaleToSize (screenWidth / 3) (screenHeight / 3)
    $ embedImage
    $ applyColorMap (fromMaybe jet $ lookup txt maps) monalisa
    , translate (-screenWidth / 6 + screenWidth * 0.005)
                (screenHeight / 6 - 0.35)
    $ scale 0.5
    $ withStrokeColor "black"
    $ withStrokeWidth (defaultStrokeWidth * 3)
    $ withFillColor "white"
    $ latex ("\\texttt{" <> txt <> "}")
    ]
  highlight <- spriteVar s 0 $ \getHighlight ->
    translate (screenWidth / 3 * x * (1 - getHighlight))
              (screenHeight / 3 * y * (1 - getHighlight))
      . scale (fromToS 1 2 getHighlight)
  return (s, highlight)
