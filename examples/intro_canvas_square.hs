#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  )
where

import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Graphics.SvgTree                hiding (Text, height)
import           Linear.Metric
import           Linear.V2
import           Linear.Vector
import           Reanimate
import           Reanimate.Builtin.Documentation
import           Geom2D.CubicBezier.Linear
import           Text.Printf

newWidth, newHeight :: Double
newWidth = 8
newHeight = 8

main :: IO ()
main = reanimate $ mapA squareViewBox $ scene $ do
  newSpriteSVG_ $ mkBackgroundPixel rtfdBackgroundColor
  newSpriteSVG_ static
  dotPath  <- newVar (QuadBezier (V2 0 0) (V2 0 0) (V2 0 0))
  dotParam <- newVar 0
  newSprite_ $ redDot <$> (evalBezier <$> unVar dotPath <*> unVar dotParam)
  let moveDot a b = do
        pos <- evalBezier <$> readVar dotPath <*> pure 1
        writeVar dotPath $ QuadBezier pos a b
        writeVar dotParam 0
        tweenVar dotParam 5 $ \v -> fromToS v 1 . curveS 2
        wait 1
  moveDot (V2 0 1)     (V2 2 1)
  moveDot (V2 (-1) 0)  (V2 (-1) 1.5)
  moveDot (V2 (0) 0)   (V2 (-2) (-3))
  moveDot (V2 (0) 0)   (V2 (2.5) (-1))
  moveDot (V2 (3) 1.5) (V2 1.5 1)
  moveDot (V2 (0) 0)   (V2 0 0)

squareViewBox :: SVG -> SVG
squareViewBox = withViewBox (-4, -4, 8, 8)

redDot :: V2 Double -> SVG
redDot (V2 x y) = translate x y $ mkGroup
  [ translate 0 (-0.5) $ scale 0.5 $ outlinedText $ T.pack $ printf "%.1f,%.1f" x y
  , withFillColor "red" $ mkCircle 0.1
  ]

static :: SVG
static = mkGroup
  [ grid
  , withStrokeColor "grey" $ mkLine (-screenWidth, 0) (screenWidth, 0)
  , withStrokeColor "grey" $ mkLine (0, -screenHeight) (0, screenHeight)
  , curlyBracket (V2 (-newWidth / 2 + defaultStrokeWidth) (newHeight / 2))
                 (V2 (newWidth / 2 - defaultStrokeWidth) (newHeight / 2))
                 1
  , translate 0 2.5 $ outlinedText "8 units"
  , curlyBracket (V2 (-newWidth / 2) (-newHeight / 2 + defaultStrokeWidth))
                 (V2 (-newWidth / 2) (newHeight / 2 - defaultStrokeWidth))
                 1
  , translate (-2.5) 0 $ rotate 90 $ outlinedText "8 units"
  ]

outlinedText :: Text -> SVG
outlinedText txt = mkGroup
  [ center
  $ withStrokeColorPixel rtfdBackgroundColor
  $ withStrokeWidth (defaultStrokeWidth * 8)
  $ withFillOpacity 0
  $ latex txt
  , center $ latex txt
  ]

curlyBracket :: RPoint -> RPoint -> Double -> SVG
curlyBracket from to height =
  withStrokeColor "black"
    $ withFillOpacity 0
    $ withStrokeWidth (defaultStrokeWidth * 2)
    $ mkPath
        [ MoveTo OriginAbsolute [from]
        , CurveTo OriginAbsolute [(from + outwards, halfway, halfway + outwards)]
        , CurveTo OriginAbsolute [(halfway, to + outwards, to)]
        ]
 where
  outwards = case normalize (from - to) ^* height of
    V2 x y -> V2 (-y) x
  halfway = lerp 0.5 from to

grid :: SVG
grid = withStrokeColor "grey" $ withStrokeWidth (defaultStrokeWidth * 0.5) $ mkGroup
  [ mkGroup
    [ translate
          0
          (i / (screenHeight) * screenHeight - screenHeight / 2 - screenHeight / 18)
        $ mkLine (-screenWidth, 0) (screenWidth, 0)
    | i <- [0 .. screenHeight]
    ]
  , mkGroup
    [ translate (i / (screenWidth) * screenWidth - screenWidth / 2) 0
        $ mkLine (0, -screenHeight) (0, screenHeight)
    | i <- [0 .. screenWidth]
    ]
  ]
