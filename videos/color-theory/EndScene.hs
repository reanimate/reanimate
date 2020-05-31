#!/usr/bin/env stack
-- stack --resolver lts-13.14 runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module EndScene (endScene) where

import           Reanimate
import           Reanimate.Builtin.Images
import Transcript

endScene :: Scene s ()
endScene = do
  newSpriteSVG_ $ scale 0.5 $ githubWhiteIcon
  waitUntil $ wordStart $ findWord ["end"] "domain"
