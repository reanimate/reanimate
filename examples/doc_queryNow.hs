#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import Reanimate
import Reanimate.Builtin.Documentation
import qualified Data.Text as T

main :: IO ()
main = reanimate $ docEnv $ scene $ do
    now <- play drawCircle *> queryNow
    play $ staticFrame 1 $ scale 2 $ withStrokeWidth 0.05 $
      mkText $ "Now=" <> T.pack (show now)
