#!/usr/bin/env stack
-- stack runghc --package reanimate
module Main(main) where

import           Codec.Picture.Types
import           Reanimate
import           Reanimate.Builtin.Documentation
import           Reanimate.Builtin.TernaryPlot

main :: IO ()
main = reanimate $ docEnv $ staticFrame 1 $
  ternaryPlot 100 $ \aCoord bCoord cCoord -> promotePixel $
    let red   = round $ aCoord*255
        green = round $ bCoord*255
        blue  = round $ cCoord*255
    in PixelRGB8 red green blue
