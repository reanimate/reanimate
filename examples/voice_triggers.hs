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
import           Graphics.SvgTree                         ( ElementRef(..) )

transcript :: Transcript
transcript = loadTranscript "voice_triggers.txt"

transformer :: SVG -> SVG
transformer =
  translate (-4) 0 . centerUsing (latex $ transcriptText transcript)

main :: IO ()
main = reanimate $ sceneAnimation $ do
  newSpriteSVG_ $ mkBackgroundPixel rtfdBackgroundColor
  waitOn $ forM_ (splitTranscript transcript) $ \(svg, tword) -> do
    let render v = transformer $ masked (wordKey tword)
                                        v
                                        svg
                                        (withFillColor "grey" $ mkRect 1 1)
                                        (withFillColor "black" $ mkRect 1 1)
    highlighted <- simpleVar render 0
    let dur = wordEnd tword - wordStart tword
    fork $ do
      wait (wordStart tword)
      tweenVar highlighted dur $ \v -> fromToS v 1
    fork $ do
      wait (wordStart tword)
      case wordReference tword of
        "one"   -> highlight dur $ latex "1"
        "two"   -> highlight dur $ latex "2"
        "three" -> highlight dur $ latex "3"
        "red"   -> highlight dur $ withFillColor "red" $ mkCircle 1
        "green" -> highlight dur $ withFillColor "green" $ mkCircle 1
        "blue"  -> highlight dur $ withFillColor "blue" $ mkCircle 1
        _       -> return ()
  wait 2
 where
  wordKey tword =
    T.unpack (wordReference tword) ++ show (wordStartOffset tword)
  highlight dur img =
    play
      $ animate
          (\t ->
            translate (screenWidth / 4) 0 $ scale t $ scaleToHeight 4 $ center
              img
          )
      # signalA (bellS 2)
      # setDuration dur

{-# INLINE masked #-}
masked :: String -> Double -> SVG -> SVG -> SVG -> SVG
masked key t maskSVG srcSVG dstSVG = mkGroup
  [ mkClipPath label $ removeGroups maskSVG
  , withClipPathRef (Ref label)
    $ translate (x - w / 2 + w * t) y (scaleToSize w screenHeight dstSVG)
  , withClipPathRef (Ref label)
    $ translate (x + w / 2 + w * t) y (scaleToSize w screenHeight srcSVG)
  ]
 where
  label         = "word-mask-" ++ key
  (x, y, w, _h) = boundingBox maskSVG
