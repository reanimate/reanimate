{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens
import Control.Monad
import Reanimate
import Reanimate.Scene

main :: IO ()
main = reanimate animation

animation :: Animation
animation = env $
  scene $ do
    symbols <-
      mapM
        oNew
        [symb_e, symb_eq, symb_m, symb_c2]
    mapM_ oShow symbols
    wait 1

    forM_ (zip symbols yPositions) $
      \(obj, yPos) -> do
        fork $
          oTweenS obj 1 $ \t -> do
            oScale %= \origin -> fromToS origin scaleFactor t
            oLeftX %= \origin -> fromToS origin screenLeft t
            oCenterY %= \origin -> fromToS origin yPos t
        wait 0.3

    wait 1

    ls <- mapM oNew [energy, equals, mass, speedOfLight]

    -- Maybe show lines with the same center as the symbols.
    -- Then show how to align the baselines.
    forM_ (zip ls yPositions) $
      \(obj, nth) -> do
        --bot <- oRead (symbols!!nth) oBottomY
        --margin <- oRead (symbols!!nth) oMarginBottom
        oModifyS obj $ do
          oLeftX .= -4
          --oTranslateY .= bot+margin
          oCenterY .= nth
        oShowWith obj oDraw

    wait 2

    forM_ ls $ \obj ->
      fork $ oHideWith obj oFadeOut

    forM_ (reverse symbols) $ \obj -> do
      fork $
        oTweenS obj 1 $ \t -> do
          oScale %= \origin -> fromToS origin 1 t
          (oTranslate . _1) %= \pos -> fromToS pos 0 t
          (oTranslate . _2) %= \pos -> fromToS pos 0 t
      wait 0.3
    wait 2

scaleFactor :: Double
scaleFactor = 0.7

symb_e :: SVG
symb_e = snd $ splitGlyphs [0] svg

symb_eq :: SVG
symb_eq = snd $ splitGlyphs [1] svg

symb_m :: SVG
symb_m = snd $ splitGlyphs [2] svg

symb_c2 :: SVG
symb_c2 = snd $ splitGlyphs [3, 4] svg

svg :: SVG
svg =
  scale 3 $
    center $
      latexAlign "E = mc^2"

energy :: SVG
energy =
  scale 1.5 $
    centerX $
      latex "Energy"

equals :: SVG
equals =
  scale 1.5 $
    centerX $
      latex "equals"

mass :: SVG
mass =
  scale 1.5 $
    centerX $
      latex "mass times"

speedOfLight :: SVG
speedOfLight =
  scale 1.5 $
    centerX $
      latex "speed of light$^2$"

yPositions :: [Double]
yPositions = [3, 1, -1, -3]

env :: Animation -> Animation
env =
  addStatic (mkBackground "lightblue")
    . mapA (withStrokeWidth defaultStrokeWidth)
    . mapA (withStrokeColor "black")
