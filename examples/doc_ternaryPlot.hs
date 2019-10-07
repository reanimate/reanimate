#!/usr/bin/env stack
-- stack runghc --package reanimate
module Main(main) where

import Reanimate hiding (raster)
import Reanimate.Builtin.Documentation
import Reanimate.Builtin.TernaryPlot
import Reanimate.Interpolate
import Data.Colour.CIE
import Codec.Picture.Types

main :: IO ()
main = reanimate $ docEnv $ animate $ const $
  ternaryPlot 1024 (\a b c -> promotePixel $ toRGB8 $ cieXYZ a b c)
