#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main (main) where

import           Reanimate
import           Reanimate.PolyShape

polygonTest :: Animation
polygonTest = mapA std $
    let shapes = svgToPolyShapes $ center $ scale 6 $ latex "$\\infty$"
        groups =
          plGroupTouching $
          map plFromPolygon $ plDecompose $ shapes
        -- totalLen = sum $ map (maximum . map (plArea.snd)) groups
        totalLen = fromIntegral $ length groups
        anis = [ setDuration dur $ animate $ \t -> renderPolyShapes $ map (plPartial' t) group
               | group <- groups
               , let groupLen = 1 -- maximum (map (plArea.snd) group)
                     dur = groupLen/totalLen * totalT ]
    in foldr andThen (pause 0) anis
  where
    totalT = 5
    std =
      withFillOpacity 1 .
      withFillColor "blue" .
      withStrokeWidth 0.00 .
      withStrokeColor "white"

main :: IO ()
main = reanimate $ bg `parA` polygonTest
  where
    bg = animate $ const $ mkBackground "black"
