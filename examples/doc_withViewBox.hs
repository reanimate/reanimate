#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Reanimate

main :: IO ()
main = reanimate $ animate $ const $
  withViewBox (0,0,1,1) $ mkBackground "yellow"
