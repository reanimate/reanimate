#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main (main) where

import           Reanimate
import           Reanimate.PolyShape
import qualified Geom2D.CubicBezier  as G

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
iShape = PolyShape $ G.ClosedPath
  [(G.Point 2 2, G.JoinLine)
  ,(G.Point 0 2, G.JoinLine)
  ,(G.Point 0 1, G.JoinLine)
  ,(G.Point 1 1, G.JoinLine)
  ,(G.Point 1 0, G.JoinLine)
  ,(G.Point 3 0, G.JoinLine)
  ,(G.Point 3 1, G.JoinLine)
  ,(G.Point 4 1, G.JoinLine)
  ,(G.Point 4 2, G.JoinLine)
  ]
