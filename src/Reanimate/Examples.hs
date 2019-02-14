{-# LANGUAGE OverloadedStrings, Arrows #-}
module Reanimate.Examples where

import Lucid.Svg
import Data.Text (Text, pack)
import Data.Monoid ((<>))
import Control.Arrow (returnA)

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
progressMeter = loop $ defineAnimation $ proc () -> do
  duration 5 -< ()
  h <- signal 0 100 -< ()
  emit -< rect_ [ width_ "30", height_ "100", stroke_ "white", stroke_width_ "2", fill_opacity_ "0" ]
  emit -< rect_ [ width_ "30", height_ (pack $ show h), stroke_ "white", fill_ "white" ]
  returnA -< ()

highlight :: Ani ()
highlight = proc () -> do
    emit -< rect_ [width_ "100%", height_ "100%", fill_ "black"]
    emit -< do
      path_ (commonAttrs "white" ++ [d_ $ renderPathText rect1])
      path_ (commonAttrs "white" ++ [d_ $ renderPathText rect2])

      path_ (commonAttrs "white" ++ [d_ $ renderPathText rect3])
      path_ (commonAttrs "lightblue" ++ [d_ $ renderPathText rect4])
      path_ (commonAttrs "yellow" ++ [d_ $ renderPathText rect5])
      path_ (commonAttrs "red" ++ [d_ $ renderPathText rect6])

    follow
      [ mkTransition highlight1 highlight2
      , mkTransition highlight2 highlight3
      , mkTransition highlight3 highlight4
      , mkTransition highlight4 highlight5
      , mkTransition highlight5 highlight6
      , mkTransition highlight6 highlight1
      ] -< ()

  where
    mkTransition from to = freeze 1 $ defineAnimation $ proc () -> do
      duration 1 -< ()
      s <- signalSCurve 2 0 1 -< ()
      let trans = morphPath from to s
      emit -<
        path_ (highlightAttrs "green" ++ [d_ $ renderPathText trans <> "Z"])
    mkRect x y width height =
      [ (x,y), (x+width, y), (x+width, y+height), (x,y+height) ]
    rect1 = mkRect margin margin w h
    rect2 = mkRect (320-margin-w*2) margin (w*2) h
    rect3 = mkRect margin (180-margin-h) w h
    rect4 = mkRect (320/3) (180-margin-h) w h
    rect5 = mkRect (320/3*2-w) (180-margin-h) w h
    rect6 = mkRect (320-margin-w) (180-margin-h) w h
    highlight1 = mkRect (margin-b) (margin-b) (w+2*b) (h+2*b)
    highlight2 = mkRect (320-margin-w*2-b) (margin-b) (w*2+2*b) (h+2*b)
    highlight3 = mkRect (320-margin-w-b) (180-margin-h-b) (w+2*b) (h+2*b)
    highlight4 = mkRect (320/3*2-w-b) (180-margin-h-b) (320/3+2*b) (h+2*b)
    highlight5 = mkRect (320/3-b) (180-margin-h-b) (320/3+2*b) (h+2*b)
    highlight6 = mkRect (margin-b) (180-margin-h-b) (320/3+2*b) (h+2*b)
    b = 7
    margin = 30
    w = 30
    h = 30
    commonAttrs c = [stroke_width_ "2", stroke_ c, fill_ c]
    highlightAttrs c = [stroke_width_ "2", stroke_ c, fill_opacity_ "0"]
