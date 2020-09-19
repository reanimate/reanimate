#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import           Control.Lens
import           Control.Monad
import           Data.Text                       (Text)
import           Reanimate
import           Reanimate.Builtin.Documentation
import           Reanimate.LaTeX
import           Reanimate.Scene

main :: IO ()
main = reanimate $ docEnv $ mapA (withFillOpacity 1) $ scene $ do
  line1 <- newLaTeX ["$>$ ", "knock!", " ", "knock!"]
  oModifyMany line1 $
    oTopY .~ screenTop

  oShowWith (line1!!0) oFadeIn
  oShow (line1!!1); wait 0.3
  oShow (line1!!3); wait 0.6

  line2 <- newLaTeX ["$>$ ", "who's there?"]
  mapM_ (`placeBelow` head line1) line2

  oShowWith (line2!!0) oFadeIn 
  oShowWith (line2!!1) oDraw

  line3 <- newLaTeX ["$>$ ", "$(\\lambda x . x)$"]
  mapM_ (`placeBelow` head line2) line3

  oShowWith (line3!!0) oFadeIn
  oShowWith (line3!!1) oFadeIn

  line4 <- newLaTeX ["$>$ ", "$(\\lambda x . x)$", " ", "who?"]
  mapM_ (`placeBelow` head line3) line4

  oShowWith (line4!!0) oFadeIn
  oShowWith (line4!!1) oFadeIn
  oShowWith (line4!!3) oDraw

  line5 <- newLaTeX ["$>$ ", "who?"]
  mapM_ (`placeBelow` head line4) line5

  oShowWith (line5!!0) oFadeIn
  oShowWith (line5!!1) oDraw

  wait 1

oModifyMany :: [Object s a] -> (ObjectData a -> ObjectData a) -> Scene s ()
oModifyMany os m = forM_ os $ \o -> oModify o m

placeBelow :: Object s a -> Object s b -> Scene s ()
placeBelow a b = do
  bBot <- oRead b oBottomY
  oModify a $
    oTopY .~ bBot

newLaTeX :: [Text] -> Scene s [Object s SVG]
newLaTeX chunks = mapM newObject $ map (translate (-4) 0) $ latexChunks chunks
