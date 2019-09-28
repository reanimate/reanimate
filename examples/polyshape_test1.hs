#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main (main) where

import           Chiphunk.Low
import           Control.Lens        ()
import           Data.List
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.Ord
import           Data.Set            (Set)
import qualified Data.Set            as Set
import           Data.Text           (Text, pack)
import           Debug.Trace
import           Geom2D.CubicBezier  (ClosedPath (..), CubicBezier (..),
                                      PathJoin (..), bezierIntersection,
                                      bezierLineIntersections, closedPathCurves,
                                      evalBezier, rotateScaleVec, transform)
import qualified Geom2D.CubicBezier  as G
import           Graphics.SvgTree (Number(Num))
import           Linear.V2
import           Numeric
import           Reanimate.Chiphunk
import           Reanimate.Constants
import           Reanimate.Driver    (reanimate)
import           Reanimate.LaTeX
import           Reanimate.Animation
import           Reanimate.PolyShape
import           Reanimate.Signal
import           Reanimate.Svg
import           System.IO.Unsafe


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
    offset = 0.2



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

squarePolyShape :: PolyShape
squarePolyShape = PolyShape $ G.ClosedPath
  [(G.Point 0 0, G.JoinLine)
  ,(G.Point 1 0, G.JoinLine)
  ,(G.Point 1 1, G.JoinLine)
  ,(G.Point 0 1, G.JoinLine)]

starPolyShape :: PolyShape
starPolyShape = PolyShape $ G.ClosedPath
  [(G.Point 0 0, G.JoinLine)
  ,(G.Point 1 2, G.JoinLine)
  ,(G.Point 2 0, G.JoinLine)
  ,(G.Point 0 1, G.JoinLine)
  ,(G.Point 2 1, G.JoinLine)
  ]

main :: IO ()
main = reanimate $ bg `sim` polygonTest
  where
    bg = animate $ const $ mkBackground "black"
