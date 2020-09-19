#!/usr/bin/env stack
-- stack runghc --package reanimate
module Main(main) where

import Reanimate
import Reanimate.Builtin.Documentation

main :: IO ()
main = reanimate $ docEnv $ scene $ do
  v <- newVar 0
  _ <- newSprite $ mkCircle <$> unVar v
  tweenVar v 1 $ \val -> fromToS val 3
  tweenVar v 1 $ \val -> fromToS val 0
