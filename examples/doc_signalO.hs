#!/usr/bin/env stack
-- stack runghc --package reanimate
module Main(main) where

import Reanimate
import Reanimate.Scene
import Reanimate.Builtin.Documentation
import Reanimate.Animation
import Control.Lens
import Control.Monad

main :: IO ()
main = reanimate $ docEnv $ scene $ do
  objs <- waitOn $ replicateM 3 $ do
    obj1 <- newObject $ mkCircle 1
    oModifyS obj1 $ oEasing .= id
    oModifyS obj1 $ oRightX .= screenRight
    fork $ oShowWith obj1 oFadeIn
    fork $ oTweenS obj1 2 $ \t ->
      oLeftX %= \origin -> fromToS origin screenLeft t
    wait 1
    oHideWith obj1 oFadeOut
    wait (-1.5)
    return obj1
  dur <- queryNow
  wait (-dur)
  forM_ objs $ \obj -> do
    signalO obj dur $ curveS 2
