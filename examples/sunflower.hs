#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Codec.Picture.Types
import qualified Data.Colour.Palette.BrewerSet as D
import qualified Diagrams.Backend.SVG          as D
import           Diagrams.Prelude              hiding (Animation, boundingBox,
                                                center, circle, duration,
                                                fontSize, rotate, scale,
                                                translate)
import qualified Diagrams.Prelude              as D
import qualified Diagrams.TwoD.Path.LSystem    as D
import           Graphics.SvgTree              (Number (..))
import           Graphics.SvgTree              as S
import           Linear.V2
import           Reanimate.Diagrams
import           Reanimate.Driver              (reanimate)
import           Reanimate.LaTeX
import           Reanimate.Animation
import           Reanimate.Signal
import           Reanimate.Svg

main :: IO ()
main = reanimate $ mkAnimation 10 $ \t ->
    let n = signalFromTo 1 500 signalLinear t
        rot = signalFromTo 0 45 signalLinear t
    in mkGroup
    [ mkBackground "black"
    , rotate rot $ translate (-320/2) (-180/2)
      (dSvg $ round n) ]
  where
    cached = [ dSvg n | n <- [0..]]
    dSvg n = renderDiagram $ withEnvelope (D.rect 320 180 :: SvgDiagram) $
      D.scale 0.2 $ sunflower n

    mkCoords :: [P2 Double]
    mkCoords =[coord (fromIntegral i) | i <- [1..]]
      where
        coord m = p2 $ fromPolar (sqrt m) (2.4 * m)
        fromPolar r theta = (r * cos theta, r * sin theta)

    floret :: Double -> SvgDiagram
    floret r = D.circle 0.6 # lw none # fc (colors !! n)
      where
        n = floor (1.4 * sqrt r) `mod` 10
        colors = black : (reverse $ D.brewerSet D.YlOrBr 9)

    sunflower :: Int ->  SvgDiagram
    sunflower n = frame 4 $ position $ take n $ zip mkCoords florets
      where
        florets = [ floret (sqrt (fromIntegral i)) | i <- [1 ..]]
