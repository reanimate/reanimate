#!/usr/bin/env stack
-- stack runghc --package reanimate
module Main (main) where

main :: IO ()
main = return ()

{-
import qualified Data.Colour.Palette.BrewerSet as D
import           Diagrams.Prelude              hiding (Animation, boundingBox,
                                                center, circle, duration,
                                                fontSize, rotate, scale,
                                                translate)
import qualified Diagrams.Prelude              as D
import           Reanimate.Diagrams
import           Reanimate hiding ((#))

main :: IO ()
main = reanimate $ mkAnimation 10 $ \t ->
    let n = fromToS 1 500 t
        rot = fromToS 0 45 t
    in mkGroup
    [ mkBackground "black"
    , rotate rot $ translate (-320/2) (-180/2)
      (dSvg $ round n) ]
  where
    dSvg n = renderDiagram $ withEnvelope (D.rect 320 180 :: SvgDiagram) $
      D.scale 0.2 $ sunflower n

    mkCoords :: [P2 Double]
    mkCoords =[coord (fromIntegral i) | i <- [1::Int ..]]
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
        florets = [ floret (sqrt (fromIntegral i)) | i <- [1::Int ..]]
-}

