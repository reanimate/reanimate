{-# LANGUAGE OverloadedStrings, Arrows #-}
module Reanimate.Examples where

import Lucid.Svg
import Data.Text (Text, pack)
import Data.Monoid ((<>))
import Control.Arrow

import Reanimate.Arrow
import Reanimate.Combinators

import Debug.Trace

test1 :: Ani ()
test1 =
  fade 1 $
  proc t -> do
    duration 10 -< t
    emit -< circle_ [cx_ "160", cy_ "90", r_ "50", fill_ "blue"]

test2 :: Ani ()
test2 = proc t -> do
  duration 5 -< t
  emit -< rect_ [width_ "100%", height_ "100%", fill_ "red", r_ (pack $ show t)]

sinewave :: Ani ()
sinewave = proc () -> do
  duration 10 -< ()
  emit -< rect_ [width_ "100%", height_ "100%", fill_ "black"]
  t <- getTime -< ()

  idx <- signalOscillate 0 1 -< ()
  let clipWidth = pack $ show $ idx * 260
  emit -< defs_ $ clipPath_ [id_ "clip"] $ rect_ [x_ "0", y_ "-90", width_ clipWidth, height_ "100%"]
  annotate (g_ [transform_ $ translate 30 90]) $
    annotate (g_ [clip_path_ "url(#clip)"]) $
      approxFn 1000 (\idx ->
        let xValue = idx*260
            yValue = sin (idx*pi*2*freq) * 50
        in (xValue, yValue)) -< ()
  emit -< line_ [x1_ "30", x2_ "30", y1_ "10", y2_ "170", stroke_ "white"]
  emit -< line_ [x1_ "30", x2_ "290", y1_ "90", y2_ "90", stroke_ "white"]

  circX <- signalOscillate 30 290 -< ()
  let circY = pack $ show $ 90 + sin (idx*pi*2*freq) * 50

  emit -< circle_ [cx_ (pack (show circX)), cy_ circY, r_ "3", fill_ "red"]
  returnA -< ()
  where
    freq = 3

morph_wave :: Ani ()
morph_wave = proc () -> do
  duration 5 -< ()
  emit -< rect_ [width_ "100%", height_ "100%", fill_ "black"]

  emit -<
    g_ [transform_ $ translate 30 50] $
    renderPath wave1
  emit -<
    g_ [transform_ $ translate 30 130] $
    renderPath wave2

  morph <- signalOscillate 0 1 -< ()
  emit -<
    g_ [transform_ $ translate 30 90] $
    renderPath $ morphPath wave1 wave2 morph

  emit -< line_ [x1_ "30", x2_ "30", y1_ "10", y2_ "170", stroke_ "white"]
  emit -< line_ [x1_ "30", x2_ "290", y1_ "90", y2_ "90", stroke_ "white"]

  returnA -< ()
  where
    myD = animationDuration morph_wave
    freq = 1/2
    wave1 = approxFnData 1000 (\idx ->
                  let xPos = idx*260
                      xValue = idx*myD
                      yValue = sin (xValue*pi*2*freq) * 20
                  in (xPos, yValue))
    wave2 = approxFnData 1000 (\idx ->
                  let xPos = idx*260
                      xValue = idx*myD
                      yValue = sin (xValue*pi*2*(freq*3)) * 20
                  in (xPos, yValue))

morph_wave_circle :: Ani ()
morph_wave_circle = proc t -> do
  duration 5 -< ()
  emit -< rect_ [width_ "100%", height_ "100%", fill_ "black"]

  idx <- signalOscillate 0 1 -< ()
  emit -<
    g_ [transform_ $ translate 30 90] $
    renderPath $ morphPath circle wave1 idx
  emit -< line_ [x1_ "30", x2_ "30", y1_ "10", y2_ "170", stroke_ "white"]
  emit -< line_ [x1_ "30", x2_ "290", y1_ "90", y2_ "90", stroke_ "white"]

  returnA -< ()
  where
    freq = 5
    wave1 = approxFnData 1000 (\idx ->
                  let xValue = idx*260
                      yValue = sin (idx*pi*2*freq) * 20
                  in (xValue, yValue))
    circle = approxFnData 1000 $ \idx ->
      (cos (idx*pi*2+pi/2)*50 + 130, sin (idx*pi*2+pi/2)*50)

progressMeters :: Ani ()
progressMeters = proc () -> do
  emit -< rect_ [width_ "100%", height_ "100%", fill_ "black"]
  annotate' progressMeter -< g_ [transform_ $ translate 40 20]
  annotate' (adjustSpeed 2 progressMeter) -< g_ [transform_ $ translate 140 20]
  annotate' (adjustSpeed 0.5 progressMeter) -< g_ [transform_ $ translate 240 20]

  emit -< do
    text_ [x_ "55", y_ "150", font_size_ "20"
          , text_anchor_ "middle"
          , fill_ "white"] "1x"
    text_ [x_ "155", y_ "150", font_size_ "20"
          , text_anchor_ "middle"
          , fill_ "white"] "2x"
    text_ [x_ "255", y_ "150", font_size_ "20"
          , text_anchor_ "middle"
          , fill_ "white"] "0.5x"

progressMeter :: Ani ()
progressMeter = defineAnimation $ proc () -> do
  duration 5 -< ()
  h <- signal 0 100 -< ()
  emit -< rect_ [ width_ "30", height_ "100", stroke_ "white", stroke_width_ "2", fill_opacity_ "0" ]
  emit -< rect_ [ width_ "30", height_ (pack $ show h), stroke_ "white", fill_ "white" ]
  returnA -< ()

highlight :: Ani ()
highlight = proc () -> do
  duration 10 -< ()
  annotate (g_ [transform_ $ scale 2 2 <> " " <> translate 25 0]) (proc t -> do
    emit -<
      g_ [transform_ $ translate 20 20] $
      g_ [transform_ $ rotateAround 45 10 10] $
      block "white"
    emit -<
      g_ [transform_ $ translate 60 20] $
      g_ [transform_ $ rotateAround 0 10 10] $
      block "white"
    emit -<
      g_ [transform_ $ translate 20 60] $
      g_ [transform_ $ rotateAround 0 10 10] $
      block "white"
    emit -<
      g_ [transform_ $ translate 20 60] $
      g_ [transform_ $ rotateAround 45 10 10] $
      block "white"
    emit -<
      g_ [transform_ $ translate 60 60] $
      g_ [transform_ $ rotateAround 45 10 10] $
      block "blue"
    t <- getTime -< ()
    emit -<
      g_ [transform_ $ translate (15 + (55-15)*(t/10)) 15] $
      rect_ [width_ "30", height_ "30", stroke_ "black", fill_opacity_ "0" ]
    ) -< ()
  returnA -< ()
  where
    block c =
      rect_ [width_ "20", height_ "20", stroke_ "black", fill_ c ]
