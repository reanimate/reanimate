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
import           Reanimate.Monad
import           Reanimate.Signal
import           Reanimate.Svg


main :: IO ()
main = reanimate $ mkAnimation 5 $ do
    s <- oscillate $ getSignal $ signalCurve 2
    emit $ mkBackground "black"
    emit $ translate (-320/2) (-180/2) $ withStrokeColor "white" $
      renderDiagram $
        withEnvelope (D.rect 320 180 :: SvgDiagram) $
        D.scale 50 $ D.translate (V2 (-2) (-0.75)) $ dia s
  where
    dia param =
        frame 0.5 $ lc white $
        mconcat
          [ lc green $ rightAngleSquare
          , tangentLine
          , baselineText "tangent" # D.translate tangentVector
          , normalLine
          , topLeftText "normal" # D.translate (-normalVector)
          ] # moveTo pt # D.fontSize large
          <> strokeLocTrail spline
      where
        pts = map p2 [(0,0), (1,1), (2,1), (3,0), (3.5,0)]

        spline :: Located (Trail V2 Double)
        spline = cubicSpline False pts

        pt = atParam spline param
        tangentVector ::  V2 Double
        tangentVector = D.normalize $ tangentAtParam spline param
        normalVector = D.normalize $ normalAtParam spline param

        symmetricLine :: V2 Double -> SvgDiagram
        symmetricLine v = fromOffsets [2 *^ v] # D.center
        tangentLine :: SvgDiagram
        tangentLine = symmetricLine tangentVector
        normalLine = symmetricLine normalVector

        rightAngleSquare :: SvgDiagram
        rightAngleSquare = square 0.1 # alignBL # D.rotate (signedAngleBetween tangentVector unitX)
