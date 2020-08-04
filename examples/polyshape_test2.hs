#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Reanimate
import           Reanimate.PolyShape

polygonTest :: Animation
polygonTest = mkAnimation 10 $ \t ->
    let s = fromToS 0.5 (-0.5) t
        bigBox = head $ svgToPolyShapes $ pathify $
          mkRect 2 2
        smallBox = head $ svgToPolyShapes $ pathify $
          translate 0 (screenHeight*s) $
          rotate (-45) $
          mkRect 1 1

        overlap = mkGroup $ map renderPolyShape [bigBox, smallBox]
        merged = translate (screenWidth/2*0.1) 0 $
          mkGroup $ map renderPolyShape $
          unionPolyShapes [bigBox, smallBox]
    in std $ gridLayout [[ overlap, merged ]]
  where
    std =
      withFillOpacity 1 .
      withFillColor "blue" .
      withStrokeWidth 0.01 .
      withStrokeColor "white"

main :: IO ()
main = reanimate $ bg `parA` polygonTest
  where
    bg = animate $ const $ mkBackground "black"
