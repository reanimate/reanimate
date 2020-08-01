module Reanimate.Constants
  ( screenWidth
  , screenHeight
  , screenTop
  , screenBottom
  , screenLeft
  , screenRight
  , defaultDPI
  , defaultStrokeWidth
  ) where

import           Graphics.SvgTree

screenWidth, screenHeight, screenTop :: Fractional a => a
screenBottom, screenLeft, screenRight :: Fractional a => a

screenWidth = 16
screenHeight = 9
screenTop = screenHeight/2
screenBottom = -screenHeight/2
screenLeft = -screenWidth/2
screenRight = screenWidth/2

defaultDPI :: Dpi
defaultDPI = 96

defaultStrokeWidth :: Double
defaultStrokeWidth = 0.05
