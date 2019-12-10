#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Lens        ()
import           Data.List
import qualified Geom2D.CubicBezier  as G
import           Reanimate
import           Reanimate.PolyShape


polygonTest :: Animation
polygonTest = animate $ \_ ->
    std $ gridLayout $ transpose
      [ test0, test1, test2, test3, test4, test5 ]
  where
    test0 = column ppA poly1
    test1 = column ppA poly2
    test2 = column ppA poly3
    test3 = column ppA poly4
    test4 = column ppB [boxPolyShape]
    test5 = column ppB [starPolyShape]

    column pp poly = map pp
      [ poly
      , map plFromPolygon $ plDecompose' 0.05 poly
      , map plFromPolygon $ plDecompose poly ]


    poly1 =
      svgToPolyShapes $ lowerTransformations $
      center $ scale 3 $
      latex "$\\Phi$"
    poly2 =
      svgToPolyShapes $ lowerTransformations $
      center $ scale 3 $
      latex "$\\vartheta$"
    poly3 =
      svgToPolyShapes $ lowerTransformations $
      center $ scale 3 $
      latex "$\\Xi$"
    poly4 =
      svgToPolyShapes $ lowerTransformations $
      center $ scale 3 $
      latex "$\\Theta$"

    ppA = renderPolyShapes
    ppB = lowerTransformations . scale 0.6 . center . renderPolyShapes
    std =
      withFillOpacity 1 .
      withFillColor "blue" .
      withStrokeWidth 0.01 .
      withStrokeColor "white"


boxPolyShape :: PolyShape
boxPolyShape = PolyShape $ G.ClosedPath
  [(G.Point 0 0, G.JoinLine)
  ,(G.Point 0 3, G.JoinLine)
  ,(G.Point 1 3, G.JoinLine)
  ,(G.Point 1 1, G.JoinLine)
  ,(G.Point 2 1, G.JoinLine)
  ,(G.Point 2 2, G.JoinLine)
  ,(G.Point 0 2, G.JoinLine)
  ,(G.Point 0 3, G.JoinLine)
  ,(G.Point 3 3, G.JoinLine)
  ,(G.Point 3 0, G.JoinLine)
  ]

{-
squarePolyShape :: PolyShape
squarePolyShape = PolyShape $ G.ClosedPath
  [(G.Point 0 0, G.JoinLine)
  ,(G.Point 1 0, G.JoinLine)
  ,(G.Point 1 1, G.JoinLine)
  ,(G.Point 0 1, G.JoinLine)]
-}

starPolyShape :: PolyShape
starPolyShape = PolyShape $ G.ClosedPath
  [(G.Point 0 0, G.JoinLine)
  ,(G.Point 1 2, G.JoinLine)
  ,(G.Point 2 0, G.JoinLine)
  ,(G.Point 0 1, G.JoinLine)
  ,(G.Point 2 1, G.JoinLine)
  ]

main :: IO ()
main = reanimate $ bg `parA` polygonTest
  where
    bg = animate $ const $ mkBackground "black"
