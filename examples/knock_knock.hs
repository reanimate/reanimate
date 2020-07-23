#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import           Control.Lens
import           Control.Monad
import           Data.Text                       (Text)
import           Reanimate
import           Reanimate.Animation             (Duration)
import           Reanimate.Builtin.Documentation
import           Reanimate.LaTeX
import           Reanimate.Scene

main :: IO ()
main = reanimate $ docEnv $ mapA (withFillOpacity 1) $ sceneAnimation $ do
  line1 <- newLaTeX ["$>$ ", "knock!", " ", "knock!"]
  oModifyMany line1 $
    oTopY .~ screenTop

  oFadeIn (line1!!0) 1
  oShow (line1!!1); wait 0.3
  oShow (line1!!3); wait 0.6

  line2 <- newLaTeX ["$>$ ", "who's there?"]
  mapM_ (`placeBelow` head line1) line2

  oFadeIn (line2!!0) 1
  oDraw (line2!!1) 1

  line3 <- newLaTeX ["$>$ ", "$(\\lambda x . x)$"]
  mapM_ (`placeBelow` head line2) line3

  oFadeIn (line3!!0) 1
  oFadeIn (line3!!1) 1

  line4 <- newLaTeX ["$>$ ", "$(\\lambda x . x)$", " ", "who?"]
  mapM_ (`placeBelow` head line3) line4

  oFadeIn (line4!!0) 1
  oFadeIn (line4!!1) 1
  oDraw (line4!!3) 1

  line5 <- newLaTeX ["$>$ ", "who?"]
  mapM_ (`placeBelow` head line4) line5

  oFadeIn (line5!!0) 1
  oDraw (line5!!1) 1

  wait 1

screenTop :: Double
screenTop = screenHeight/2

oModifyMany :: [Object s a] -> (ObjectData a -> ObjectData a) -> Scene s ()
oModifyMany os m = forM_ os $ \o -> oModify o m

placeBelow :: Object s a -> Object s b -> Scene s ()
placeBelow a b = do
  bBot <- oRead b oBottomY
  oModify a $
    oTopY .~ bBot

newLaTeX :: [Text] -> Scene s [Object s SVG]
newLaTeX chunks = mapM newObject $ map (translate (-4) 0) $ latexChunks chunks

oDraw :: Object s a -> Duration -> Scene s ()
oDraw o d = do
  oShow o
  oTweenS o d $ \t ->
    oContext .= withFillOpacity (max 0 $ t*10-9) . partialSvg t
