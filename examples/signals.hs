#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main (main) where

import           Data.Monoid         ((<>))
import           Data.Text           (Text, pack)
import           Graphics.SvgTree    hiding (Text)
import           Numeric
import           Reanimate.Animation
import           Reanimate.Constants
import           Reanimate.Driver    (reanimate)
import           Reanimate.LaTeX
import           Reanimate.Signal
import           Reanimate.Svg

main :: IO ()
main = reanimate $ pauseAtEnd 5 $
    curvesExample (\_ -> ([], "[]"))
      `seqA`
    curvesExample (\s ->
      ( [(1, constantS s)]
      , "[(1, constantS " <> ppD s <> ")]"))
      `seqA`
    curvesExample (\s ->
      ( [(s, constantS 0), (1, id)]
      , "[(" <> ppD s <> ", constantS 0), (1, id)]"))
      `seqA`
    curvesExample (\s ->
      ( [(s, constantS 1), (1, reverseS)]
      , "[(" <> ppD s <> ", constantS 1), (1, reverseS)]"))
      `seqA`
    curvesExample (\s ->
      ( [(1, curveS (2+s*3))]
      , "[(1, curveS "<> ppD (2+s*3) <>")]"))
      `seqA`
    curvesExample (\s ->
      ( [(1, fromToS s 1 . curveS 5)]
      , "[(1, fromToS "<>  ppD s <>" 1 \\$ curveS 5)]"))
      `seqA`
    curvesExample (\s ->
      ( [(1, bellS (2+s*3))]
      , "[(1, bellS "<> ppD (2+s*3)<>")]"))
  where
    ppD s = pack (showFFloat (Just 2) s "")

convertX, convertY :: Double -> Double
convertX x = x*(screenWidth/320)
convertY y = y*(screenHeight/180)

curvesExample :: (Double -> ([(Double, Double -> Double)], Text)) -> Animation
curvesExample gen = mkAnimation 2 $ \t ->
    mkGroup
    [ mkBackground "black"
    , withFillColor "white" $
      translate 0 (screenHeight*0.35) $
      center $ latex "Signals"
    , let (curveFns, name) = gen t in
      center $
      mkGroup
        [ withStrokeColor "white" $ withStrokeWidth 0.01 $
          mkGroup
          [ mkLine (0, 0)
                   (convertX 200, 0)
          , mkLine (0, 0)
                   (0, convertY 50) ]
        , withStrokeColor "white" $ withStrokeWidth 0.01 $
          mkGroup
          [ mkLine (0, convertX $ y)
                   (convertX 200, convertX y)
          | y <- [10,20,30,40,50] ]
        , withFillColor "white" $
          mkGroup
            [ translate (convertX $ -5) (convertX $ -5)  $ scale 0.5 $ center $ latex "0"
            , translate (convertX $ -5) (convertX $ 50)  $ scale 0.5 $ center $ latex "1"
            , translate (convertX $ 205) (convertX $ -5) $ scale 0.5 $ center $ latex "1"
            , translate (convertX $ 100) (convertX $ -30)$ scale 0.6 $ center $ latex name ]
        , withFillOpacity 0 $ withStrokeColor "green" $ -- withStrokeWidth 0.5 $
          lowerTransformations $ scaleXY (convertX $ 200) (convertX $ (50)) $ mkSignalLine (fromListS curveFns)
        ]
    ]

mkSignalLine :: Signal -> Tree
mkSignalLine fn = mkLinePath
    [ (x/steps, fn (x/steps))
    | x <- [0 .. steps] ]
  where
    steps = 1000
