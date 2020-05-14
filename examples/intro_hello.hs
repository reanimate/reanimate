#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
import           Reanimate

main :: IO ()
main = reanimate $ addStatic (mkBackground "cyan") $ staticFrame 1 $ mkText "Hello world"
