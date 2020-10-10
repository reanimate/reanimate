#!/usr/bin/env stack
-- stack runghc --package reanimate
module Main(main) where

import Reanimate
import Reanimate.External
import Codec.Picture.Types

main :: IO ()
main = reanimate $ staticFrame 1 $
  let icon = "cplusplus" in mkGroup
  [ mkBackgroundPixel (promotePixel $ simpleIconColor icon)
  , withFillOpacity 1 $ simpleIcon icon ]
