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
polygonTest = mkAnimation 10 $ \t ->
    let s = signalFromTo 0.5 (-0.5) signalLinear t
        bigBox = head $ svgToPolyShapes $ pathify $
          mkRect 2 2
        smallBox = head $ svgToPolyShapes $ pathify $
          translate (0) (screenHeight*s) $
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
      withStrokeWidth (Num 0.01) .
      withStrokeColor "white"




main :: IO ()
main = reanimate $ bg `sim` polygonTest
  where
    bg = animate $ const $ mkBackground "black"
