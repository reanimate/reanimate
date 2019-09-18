#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
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
import           Reanimate.Monad
import           Reanimate.Signal
import           Reanimate.Svg


main :: IO ()
main = reanimate $ repeatAnimation 5 $ mkAnimation 5 $ do
    s <- getSignal signalLinear
    emit $ mkBackground "black"
    emit $ scale (2/50) $ center $ -- translate (-320/2) (-180/2) $
      withStrokeColor "white" $
      renderDiagram $
        withEnvelope (D.rect 320 180 :: SvgDiagram) $
        D.scale 50 $
        lc white $
        example s

vectorField (x, y) = r2 (sin (y + 1), sin (x + 1))

arrowAtPoint (x, y) = arrowAt' opts (p2 (x, y)) (sL *^ vf) # alignTL
  where
    vf   = vectorField (x, y)
    m    = norm $ vectorField (x, y)

-- Head size is a function of the length of the vector
-- as are tail size and shaft length.

    hs   = 0.02 * m
    sW   = 0.004 * m
    sL   = 0.05 + 0.1 * m
    opts = (with & arrowHead  .~ spike
                 & headLength .~ normalized hs
                 & shaftStyle %~ lwN sW)

example :: Double -> SvgDiagram
example n = field
  where
    xOffset = cos (2*pi*n)
    yOffset = sin (2*pi*n)
    locs   = [(x+xOffset, y+yOffset) | x <- [0.1, 0.3 .. 3.25], y <- [0.1, 0.3 .. 3.25]]

    points = map p2 locs

    arrows = map arrowAtPoint locs
    field   = position $ zip points arrows
