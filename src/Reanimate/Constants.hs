module Reanimate.Constants
  ( screenWidth
  , screenHeight
  , defaultDPI
  , defaultStrokeWidth
  ) where

import           Graphics.SvgTree

screenWidth, screenHeight :: Num a => a

screenWidth = 16
screenHeight = 9

defaultDPI :: Dpi
defaultDPI = 96

defaultStrokeWidth :: Double
defaultStrokeWidth = 0.05
