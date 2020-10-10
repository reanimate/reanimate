#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import Reanimate
import Reanimate.Scene
import Control.Lens

main :: IO ()
main = reanimate $ scene $ do
  newSpriteSVG_ $ mkBackground "white"
  logo <- oNew $ center $ latex "Reanimate"
  oModify logo $ oScale .~ 3

  oShowWith logo oFadeIn
  oTweenS logo 1 $ \t -> do
    oScale %= \prev -> fromToS prev 2 t
    oTopY %= \prev -> fromToS prev screenTop t

  haskell <- oNew $ center $ withStrokeColor "black" $ latex "Haskell"
  oModify haskell $ oScale .~ 3
  oShowWith haskell oDraw
  oTweenS haskell 1 $ \t -> do
    oScale %= \prev -> fromToS prev 2 t
    oBottomY %= \prev -> fromToS prev screenBottom t

  features <- mapM (oNew . scale 3 . center . latex)
    ["SVG", "\\LaTeX", "Animation", "Windows", "Linux", "MacOS", "Browsers"]
  oShowWith (head features) $ adjustDuration (*3) . oScaleIn
  featureChain features

  wait (-1)
  fork $ oHideWith logo oFadeOut
  fork $ oHideWith haskell oFadeOut

replace :: Object s a -> Object s b -> Scene s ()
replace a b = do
  fork $ oHideWith a $ adjustDuration (*3) . oScaleOut
  wait 0.2
  oShowWith b $ adjustDuration (*3) . oScaleIn

featureChain :: [Object s a] -> Scene s ()
featureChain (x:y:xs) = do
    replace x y
    featureChain (y:xs)
featureChain [x] = do
  oHideWith x $ adjustDuration (*3) . oScaleOut
featureChain [] = return ()
