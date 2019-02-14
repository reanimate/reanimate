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
sinewave = proc t -> do
  duration 5 -< ()
  emit -< rect_ [width_ "100%", height_ "100%", fill_ "black"]
  let clipWidth = pack $ show $ t/getDuration sinewave * 260
  emit -< defs_ $ clipPath_ [id_ "clip"] $ rect_ [x_ "0", y_ "-90", width_ clipWidth, height_ "100%"]
  annotate (g_ [transform_ $ translate 30 90]) $
    annotate (g_ [clip_path_ "url(#clip)"]) $
      approxFn 1000 (\idx ->
        let xPos = idx*260
            xValue = idx*getDuration sinewave
            yValue = sin (xValue*pi*2*freq) * 50
        in (xPos, yValue)) -< t
  emit -< line_ [x1_ "30", x2_ "30", y1_ "10", y2_ "170", stroke_ "white"]
  emit -< line_ [x1_ "30", x2_ "290", y1_ "90", y2_ "90", stroke_ "white"]

  let circX = pack $ show $ 30 + t/getDuration sinewave * (290-30)
      circY = pack $ show $ 90 + sin (t*pi*2*freq) * 50

  emit -< circle_ [cx_ circX, cy_ circY, r_ "3", fill_ "red"]
  returnA -< ()
  where
    freq = 1/2

morph_wave :: Ani ()
morph_wave = proc t -> do
  duration 5 -< ()
  emit -< rect_ [width_ "100%", height_ "100%", fill_ "black"]

  annotate (g_ [transform_ $ translate 30 50]) $
    (proc t -> emit -< renderPath wave1) -< t
  annotate (g_ [transform_ $ translate 30 130]) $
    (proc t -> emit -< renderPath wave2) -< t
  annotate (g_ [transform_ $ translate 30 90]) $
    (proc t -> do
      let idx = if t/myD*2 < 1 then t/myD*2
                else (2 - t/myD*2)
      emit -< renderPath $ morphPath wave1 wave2 idx) -< t
  emit -< line_ [x1_ "30", x2_ "30", y1_ "10", y2_ "170", stroke_ "white"]
  emit -< line_ [x1_ "30", x2_ "290", y1_ "90", y2_ "90", stroke_ "white"]

  returnA -< ()
  where
    myD = getDuration morph_wave
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

  annotate (g_ [transform_ $ translate 30 90]) $
    (proc t -> do
      let idx = if t/myD*2 < 1 then t/myD*2
                else (2 - t/myD*2)
      emit -< renderPath $ morphPath wave1 circle idx) -< t
  emit -< line_ [x1_ "30", x2_ "30", y1_ "10", y2_ "170", stroke_ "white"]
  emit -< line_ [x1_ "30", x2_ "290", y1_ "90", y2_ "90", stroke_ "white"]

  returnA -< ()
  where
    myD = getDuration morph_wave
    freq = 1/2
    wave1 = approxFnData 1000 (\idx ->
                  let xPos = idx*260
                      xValue = idx*myD
                      yValue = sin (xValue*pi*2*freq) * 20
                  in (xPos, yValue))
    circle = approxFnData 1000 $ \idx ->
      (cos (idx*pi*2+pi/2)*50 + 130, sin (idx*pi*2+pi/2)*50)
