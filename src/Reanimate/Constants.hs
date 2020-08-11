{- |
  Reanimate configures a consistent, default canvas. The values of this default
  can be observed via the constants in this module. Keep in mind, these values
  describe the /default/ canvas and will not apply to custom viewports.
-}
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

-- | Number of units from the left-most point to the right-most point on the screen.
screenWidth :: Fractional a => a

-- | Number of units from the bottom to the top of the screen.
screenHeight :: Fractional a => a

-- | Position of the top of the screen.
screenTop :: Fractional a => a

-- | Position of the bottom of the screen.
screenBottom :: Fractional a => a

-- | Position of the left side of the screen.
screenLeft :: Fractional a => a

-- | Position of the right side of the screen.
screenRight :: Fractional a => a

screenWidth = 16
screenHeight = 9
screenTop = screenHeight/2
screenBottom = -screenHeight/2
screenLeft = -screenWidth/2
screenRight = screenWidth/2

-- | SVG allows measurements in inches which have to be converted to local units.
--   This value describes how many local units there are in an inch.
defaultDPI :: Dpi
defaultDPI = 96

-- | Default thickness of lines.
defaultStrokeWidth :: Double
defaultStrokeWidth = 0.05
