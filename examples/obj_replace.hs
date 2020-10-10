#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import Reanimate
import Reanimate.Scene

main :: IO ()
main = reanimate $ scene $ do
  newSpriteSVG_ $ mkBackground "white"
  t1 <- oNew $ centerX $ scale 3 $ latex "reanimate"
  t2 <- oNew $ centerX $ scale 3 $ latex "and \\LaTeX"
  
  oShow t1
  replace t1 t2; wait 1
  replace t2 t1; wait 1

replace :: Object s a -> Object s b -> Scene s ()
replace a b = do
  fork $ oHideWith a $ adjustDuration (*3) . oScaleOut
  wait 0.2
  oShowWith b $ adjustDuration (*3) . oScaleIn