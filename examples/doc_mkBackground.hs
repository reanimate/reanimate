#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Reanimate
import           Reanimate.Builtin.Documentation

main :: IO ()
main = reanimate $ animate $ const $
  mkBackground "yellow"
