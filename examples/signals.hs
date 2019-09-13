#!/usr/bin/env stack
-- stack --resolver lts-13.14 runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import           Control.Lens
import           Data.Text (pack, Text)
import           Numeric
import           Data.Monoid

import           Graphics.SvgTree hiding (Text)
import           Reanimate.Driver (reanimate)
import           Reanimate.LaTeX
import           Reanimate.Monad
import           Reanimate.Svg
import           Reanimate.Signal

main :: IO ()
main = reanimate $ pauseAtEnd 5 $
    curvesExample (\_ -> ([], "[]"))
      `before`
    curvesExample (\s ->
      ( [(1, signalFlat s)]
      , "[(1, signalFlat " <> showFloat s <> ")]"))
      `before`
    curvesExample (\s ->
      ( [(s, signalFlat 0), (1, signalLinear)]
      , "[(" <> showFloat s <> ", signalFlat 0), (1, signalLinear)]"))
      `before`
    curvesExample (\s ->
      ( [(s, signalFlat 1), (1, signalReverse signalLinear)]
      , "[(" <> showFloat s <> ", signalFlat 0), (1, signalReverse signalLinear)]"))
      `before`
    curvesExample (\s ->
      ( [(1, signalCurve (2+s*3))]
      , "[(1, signalCurve "<> showFloat (2+s*3) <>")]"))
      `before`
    curvesExample (\s ->
      ( [(1, signalFromTo s 1 $ signalCurve 5)]
      , "[(1, signalFromTo "<>  showFloat s <>" 1 \\$ signalCurve 5)]"))
      `before`
    curvesExample (\s ->
      ( [(1, signalBell (2+s*3))]
      , "[(1, signalBell "<> showFloat (2+s*3)<>")]"))
  where
    showFloat s = pack (showFFloat (Just 2) s "")

curvesExample :: (Double -> ([(Double, Double -> Double)], Text)) -> Animation
curvesExample gen = mkAnimation 2 $ do
    emit $ mkBackground "black"
    emit $ withFillColor "white" $
      translate 0 (-70) $
      scale 2 $ center $ latex "Signals"
    s <- getSignal signalLinear
    let (curveFns, name) = gen s
    emit $
        center $
        mkGroup
        [ withStrokeColor "white" $ withStrokeWidth (Num 0.5) $
          mkGroup
          [ mkLine (Num 0, Num 0)
                   (Num 200, Num 0)
          , mkLine (Num $ 0, Num 0)
                   (Num $ 0, Num $ -50) ]
        , withStrokeColor "white" $ withStrokeWidth (Num 0.1) $
          mkGroup
          [ mkLine (Num 0, Num $ -y)
                   (Num 200, Num $ -y)
          | y <- [10,20,30,40,50] ]
        , withFillColor "white" $
          mkGroup
            [ translate (-5) 5     $ center $ latex "0"
            , translate (-5) (-50) $ center $ latex "1"
            , translate (205) 5    $ center $ latex "1"
            , translate 100 20     $ center $ latex name ]
        , withFillOpacity 0 $ withStrokeColor "green" $ withStrokeWidth (Num 0.5) $
          lowerTransformations $ scaleXY 200 (-50) $ mkSignalLine (signalFromList curveFns)
        ]

mkSignalLine :: Signal -> Tree
mkSignalLine fn = mkLinePath
    [ (x/steps, fn (x/steps))
    | x <- [0 .. steps] ]
  where
    steps = 1000
