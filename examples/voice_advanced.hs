#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}
module Main where

import           Control.Monad
import qualified Data.Text                     as T
import           Reanimate
import           Reanimate.Voice
import           Reanimate.Builtin.Documentation
import           Geom2D.CubicBezier                       ( QuadBezier(..)
                                                          , evalBezier
                                                          , Point(..)
                                                          )
import           Graphics.SvgTree                         ( ElementRef(..) )

transcript :: Transcript
transcript = loadTranscript "voice_advanced.txt"

main :: IO ()
main = reanimate $ scene $ do
  bg <- newSpriteSVG $ mkBackgroundPixel rtfdBackgroundColor
  spriteZ bg (-100)
  newSpriteSVG_ $ mkGroup
    [withStrokeColor "black" $ mkLine (-screenWidth, 0) (screenWidth, 0)]

  centerTxt <- textHandler

  flashEffect
  circleEffect
  squareEffect
  finalEffect

  waitOn $ forM_ (transcriptWords transcript) $ \tword -> fork $ do
    wait (wordStart tword)
    writeVar centerTxt $ wordReference tword

  wait 2

wordDuration :: TWord -> Double
wordDuration tword = wordEnd tword - wordStart tword

--
finalEffect :: Scene s ()
finalEffect = fork $ do
  let begin  = findWord transcript ["final"] "circles"
      ends   = findWords transcript ["final"] "flash"
      path   = QuadBezier (Point 6 (-radius)) (Point 0 6) (Point (-6) (-radius))
      radius = 0.3
  wait (wordStart begin)
  ss <- fork $ replicateM 3 (circleSprite radius path <* wait 0.2)
  mapM_ (flip spriteZ (-1)) ss
  forM_ (zip ss ends) $ \(s, end) -> fork $ do
    spriteMap s flipXAxis
    wait (wordStart end - wordStart begin)
    destroySprite s

-- square effect
squareEffect :: Scene s ()
squareEffect = fork $ do
  let
    begin = findWord transcript [] "square"
    end   = findWord transcript ["middle"] "square"
    path =
      QuadBezier (Point 6 (-size / 2)) (Point 0 6) (Point (-6) (-size / 2))
    size = 1
  wait (wordStart begin)
  s <- squareSprite size path
  spriteMap s (rotate 180)
  spriteZ s (-1)
  wait (wordStart end + wordDuration end / 2 - wordStart begin)
  destroySprite s

-- circle effect
circleEffect :: Scene s ()
circleEffect = fork $ do
  let begin  = findWord transcript [] "circle"
      end    = findWord transcript ["middle"] "circle"
      path   = QuadBezier (Point 6 (-radius)) (Point 0 6) (Point (-6) (-radius))
      radius = 0.3
  wait (wordStart begin)
  s <- circleSprite radius path
  spriteZ s (-1)
  wait (wordStart end + wordDuration end / 2 - wordStart begin)
  destroySprite s

-- flash effect
flashEffect :: Scene s ()
flashEffect = forM_ (findWords transcript [] "flash") $ \flashWord -> fork $ do
  wait (wordStart flashWord)
  flash <- newSpriteSVG $ mkBackground "black"
  spriteTween flash (wordDuration flashWord)
    $ \t -> withGroupOpacity (fromToS 0 0.7 $ (powerS 2 . reverseS) t)
  wait (wordDuration flashWord)
  destroySprite flash

--------------------------------------------------------------------------
-- Helpers and sprites

textHandler :: Scene s (Var s T.Text)
textHandler = simpleVar render T.empty
 where
  render txt =
    let txtSvg      = translate 0 (-0.25) $ centerX $ latex txt
        activeWidth = svgWidth txtSvg + 0.5
    in  mkGroup
          [ withStrokeWidth 0 $ withFillColorPixel rtfdBackgroundColor $ mkRect
            activeWidth
            1
          , txtSvg
          , withStrokeColor "black"
            $ mkLine (activeWidth / 2, 0.5) (activeWidth / 2, -0.5)
          , withStrokeColor "black"
            $ mkLine (-activeWidth / 2, 0.5) (-activeWidth / 2, -0.5)
          ]

circleSprite :: Double -> QuadBezier Double -> Scene s (Sprite s)
circleSprite radius path = newSprite $ do
  t <- spriteT
  d <- spriteDuration
  pure
    $ let Point x y = evalBezier path (t / d)
      in  mkGroup
            [ mkClipPath "circle-mask"
            $ removeGroups
            $ translate 0 (screenHeight / 2)
            $ withFillColorPixel rtfdBackgroundColor
            $ mkRect screenWidth screenHeight
            , withClipPathRef (Ref "circle-mask") $ translate x y $ mkCircle
              radius
            ]

squareSprite :: Double -> QuadBezier Double -> Scene s (Sprite s)
squareSprite size path = newSprite $ do
  t <- spriteT
  d <- spriteDuration
  pure
    $ let Point x y = evalBezier path (t / d)
      in  mkGroup
            [ mkClipPath "square-mask"
            $ removeGroups
            $ translate 0 (screenHeight / 2)
            $ withFillColorPixel rtfdBackgroundColor
            $ mkRect screenWidth screenHeight
            , withClipPathRef (Ref "square-mask")
            $ translate x y
            $ rotate (t / d * 360)
            $ withFillOpacity 0
            $ withStrokeColor "black"
            $ mkRect size size
            ]
