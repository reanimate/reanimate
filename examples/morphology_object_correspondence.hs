#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp  #-}
module Main(main) where

import           Codec.Picture
import           Control.Lens           ((&))
import           Reanimate
import           Reanimate.Morph.Common
import           Reanimate.Morph.Linear

bgColor :: PixelRGBA8
bgColor = PixelRGBA8 252 252 252 0xFF

main :: IO ()
main = reanimate $
  addStatic (mkBackgroundPixel bgColor) $
  mapA (withStrokeWidth 0) $
  mapA (withStrokeColor "black") $
  mapA (withFillOpacity 1) $
    scene $ do
      let showLabel label = do
            fork $ play $ staticFrame 4 (center $ latex label)
              & mapA (translate 0 4)
              & applyE (overBeginning 0.1 fadeInE)
              & applyE (overEnding 0.1 fadeOutE)
      let sides = mkGroup
            [ translate (-4) 0 $ withFillColor "blue" $
              mkCircle 2
            , translate 4 0 $ withFillColor "green" $
              mkCircle 2]
          middle =
            withFillColor "red" $
            mkRect 4 4

      showLabel "Cut"
      play $ step sides middle
        & pauseAtEnd 1
      play $ step middle sides
        & pauseAtEnd 1

      showLabel "Overlap"
      play $ stepDup sides middle
        & pauseAtEnd 1
      play $ stepDup middle sides
        & pauseAtEnd 1

      showLabel "Obliterate"
      play $ stepGenesis sides middle
        & pauseAtEnd 1
      play $ stepGenesis middle sides
        & pauseAtEnd 1
  where
    step from to =
      signalA (curveS 2) $ animate $ morph linear from to
    stepDup from to =
      signalA (curveS 2) $ animate $ morph linear{morphObjectCorrespondence=dupObjectCorrespondence} from to
    stepGenesis from to =
      signalA (curveS 2) $ animate $ morph linear{morphObjectCorrespondence=genesisObjectCorrespondence} from to
