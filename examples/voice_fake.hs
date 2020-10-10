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
transcript =
  fakeTranscript
    "There is no audio\n\n\
    \for this transcript....\n\n\n\
    \Timings are fake,\n\n\
    \which is quite useful\n\n\
    \during development"

main :: IO ()
main = reanimate $ scene $ do
  newSpriteSVG_ $ mkBackgroundPixel rtfdBackgroundColor
  waitOn $ forM_ (splitTranscript transcript) $ \(svg, tword) -> do
    highlighted <- newVar 0
    void $ newSprite $ do
      v <- unVar highlighted
      pure $ centerUsing (latex $ transcriptText transcript) $ masked
        (wordKey tword)
        v
        svg
        (withFillColor "grey" $ mkRect 1 1)
        (withFillColor "black" $ mkRect 1 1)
    fork $ do
      wait (wordStart tword)
      let dur = wordEnd tword - wordStart tword
      tweenVar highlighted dur $ \v -> fromToS v 1
  wait 2
 where
  wordKey tword =
    T.unpack (wordReference tword) ++ show (wordStartOffset tword)

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
