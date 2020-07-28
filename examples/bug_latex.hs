#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import Reanimate
import Reanimate.Builtin.Documentation
import Reanimate.Morph.Common
import Reanimate.Morph.Linear

main :: IO ()
main = reanimate $ docEnv $
    staticFrame 1 $ latex "\\textbf{$\\textsf{2}^{\\textsf{16}}$}"
