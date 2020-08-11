#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Linear.V2
import           Reanimate
import           Reanimate.Internal.CubicBezier
import           Reanimate.PolyShape

polygonTest :: Animation
polygonTest = mkAnimation 10 $ \t ->
    let original =
          renderPolyShapes [iShape]
        originalP =
          renderPolyShapes $ map (plPartial t) [iShape]
        decomposed =
          renderPolyShapes $
          map plFromPolygon $ plDecompose [iShape]
        decomposedP =
          renderPolyShapes $ map (plPartial' t) $ concat $ plGroupTouching $
          map plFromPolygon $ plDecompose $ [iShape]
    in std $ gridLayout
      [[original, originalP]
      ,[decomposed, decomposedP]]
  where
    std =
      withFillOpacity 1 .
      withFillColor "blue" .
      withStrokeWidth 0.02 .
      withStrokeColor "white"

main :: IO ()
main = reanimate $ bg `parA` polygonTest
  where
    bg = animate $ const $ mkBackground "black"

iShape :: PolyShape
iShape = PolyShape $ ClosedPath
  [(V2 2 2, JoinLine)
  ,(V2 0 2, JoinLine)
  ,(V2 0 1, JoinLine)
  ,(V2 1 1, JoinLine)
  ,(V2 1 0, JoinLine)
  ,(V2 3 0, JoinLine)
  ,(V2 3 1, JoinLine)
  ,(V2 4 1, JoinLine)
  ,(V2 4 2, JoinLine)
  ]
