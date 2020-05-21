#!/usr/bin/env stack
-- stack runghc --package reanimate
module Main (main) where

main :: IO ()
main = return ()

{-
import           Diagrams.Prelude              hiding (Animation, boundingBox,
                                                center, circle, duration,
                                                fontSize, rotate, scale,
                                                translate)
import qualified Diagrams.Prelude              as D
import           Reanimate.Diagrams
import           Reanimate.Driver              (reanimate)
import           Reanimate.Animation
import           Reanimate.Ease
import           Reanimate.Svg


main :: IO ()
main = reanimate $ playThenReverseA $ mkAnimation 5 $ \t ->
    let s = curveS 2 t in
    mkGroup
    [ mkBackground "black"
    , scale (2/50) $ scaleXY 1 (-1) $
      translate (-320/2) (-180/2) $ withStrokeColor "white" $
      renderDiagram $
        withEnvelope (D.rect 320 180 :: SvgDiagram) $
        D.scale 50 $ D.translate (V2 (-2) (-0.75)) $ dia s ]
  where
    dia param =
        frame 0.5 $ lc white $
        mconcat
          [ lc green $ rightAngleSquare
          , tangentLine
          , fc white $ baselineText "tangent" # D.translate tangentVector
          , normalLine
          , fc white $ topLeftText "normal" # D.translate (-normalVector)
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
-}

