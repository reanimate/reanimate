{-# LANGUAGE OverloadedStrings, Arrows, ParallelListComp #-}
module Reanimate.Examples where

import Lucid.Svg
import Data.Text (Text, pack)
import Data.Monoid ((<>))
import Control.Arrow (returnA)

import Reanimate.Arrow
import Reanimate.Combinators

import Debug.Trace

sinewave :: Ani ()
sinewave = proc () -> do
    duration 10 -< ()
    emit -< rect_ [width_ "100%", height_ "100%", fill_ "black"]

    idx <- signalOscillate 0 1 -< ()
    emit -< do
      defs_ $ clipPath_ [id_ "clip"] $
        rect_ [x_ "0", num_ y_ (-height), num_ width_ (idx * width), height_ "100%"]
      g_ [transform_ $ translate margin height, clip_path_ "url(#clip)"] $
        renderPath $ approxFnData 1000 wave
      line_ [ num_ x1_ margin, num_ x2_ margin, y1_ "10", y2_ "170"
            , stroke_ "white"]
      line_ [num_ x1_ margin, num_ x2_ (margin+width), num_ y1_ height, num_ y2_ height
            , stroke_ "white"]

    circX <- signalOscillate margin (width+margin) -< ()
    let circY = height + sin (idx*pi*2*freq) * 50
    emit -< circle_ [num_ cx_ circX, num_ cy_ circY, r_ "3", fill_ "red"]
  where
    freq = 3; margin = 30; width = 260; height = 90
    wave idx = (idx*width, sin (idx*pi*2*freq) * 50)

morph_wave :: Ani ()
morph_wave = proc () -> do
    duration 5 -< ()
    emit -< rect_ [width_ "100%", height_ "100%", fill_ "black"]

    morph <- signalOscillate 0 1 -< ()
    emit -< do
      g_ [transform_ $ translate 30 50] $ renderPath wave1
      g_ [transform_ $ translate 30 130] $ renderPath wave2
      g_ [transform_ $ translate 30 90] $ renderPath $ morphPath wave1 wave2 morph
      line_ [x1_ "30", x2_ "30", y1_ "10", y2_ "170", stroke_ "white"]
      line_ [x1_ "30", x2_ "290", y1_ "90", y2_ "90", stroke_ "white"]
  where
    freq = 3; width = 260
    wave1 = approxFnData 1000 $ \idx -> (idx*width, sin (idx*pi*2*freq) * 20)
    wave2 = approxFnData 1000 $ \idx -> (idx*width, sin (idx*pi*2*(freq*3)) * 20)

morph_wave_circle :: Ani ()
morph_wave_circle = proc t -> do
  duration 5 -< ()
  emit -< rect_ [width_ "100%", height_ "100%", fill_ "black"]

  idx <- signalOscillate 0 1 -< ()
  emit -< do
    g_ [transform_ $ translate 30 90] $
      renderPath $ morphPath circle wave1 idx
    line_ [x1_ "30", x2_ "30", y1_ "10", y2_ "170", stroke_ "white"]
    line_ [x1_ "30", x2_ "290", y1_ "90", y2_ "90", stroke_ "white"]
  where
    freq = 5; width = 260; radius = 50
    wave1 = approxFnData 1000 $ \idx -> (idx*width, sin (idx*pi*2*freq) * 20)
    circle = approxFnData 1000 $ \idx ->
      (cos (idx*pi*2+pi/2)*radius + width/2, sin (idx*pi*2+pi/2)*radius)

progressMeters :: Ani ()
progressMeters = proc () -> do
  emit -< rect_ [width_ "100%", height_ "100%", fill_ "black"]
  annotate' (adjustSpeed 1.0 progressMeter) -< g_ [transform_ $ translate 40 20]
  annotate' (adjustSpeed 2.0 progressMeter) -< g_ [transform_ $ translate 140 20]
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
  emit -< rect_ [ width_ "30", num_ height_ h, stroke_ "white", fill_ "white" ]
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

clip_rect :: Ani ()
clip_rect = proc () -> do
  emit -< rect_ [width_ "100%", height_ "100%", fill_ "black"]
  follow
    [ proc () -> do
      sim [ paintStatic prev | prev <- [max 0 (n-4) .. n-1] ] -< ()
      sim [ runAni "black" i | i <- [n-4], i>=0 ] -< ()
      runAni "white" n -< ()
    | n <- [0..15]
    ] -< ()
  where
    paintStatic nth = proc () ->
      annotate' (obj "white" (20+nth*10) (20+nth*10))
        -< g_ [transform_ $ translate 160 90]
    runAni color nth = defineAnimation $ proc () ->
      annotate' (circle_clip (obj color (20+nth*10) (20+nth*10)))
        -< g_ [transform_ $ translate 160 90]
    obj c width height = proc () -> do
      duration 1 -< ()
      emit -< rect_ [ num_ width_ width, num_ height_ height
            , num_ x_ (-width/2), num_ y_ (-height/2)
            , stroke_ c, fill_opacity_ "0", stroke_width_ "2" ]

circle_clip :: Ani () -> Ani ()
circle_clip sub = proc () -> do
    arc <- signal (pi*2) 0 -< ()
    let startX = pack$show$sin 0 * 1000
        startY = pack$show$cos 0 * 1000
        xPos = pack$show$sin arc * 1000
        yPos = pack$show$cos arc * 1000
        long = if arc < pi then "1" else "0"
    emit -< defs_ $ clipPath_ [id_ $ uniqName] $
      path_ [ stroke_ "white", fill_ "white"
            , d_ $ "M "<>startX<>" "<>startY<>" A 1000 1000 0 "<>long<>" 1 "
                  <>xPos<> " "<>yPos<>" L 0 0 Z"]
    annotate' sub -<
      g_ [clip_path_ $ "url(#"<>uniqName<>")"]
  where
    uniqName = "clip" -- XXX: Not very unique?


scaling :: Ani ()
scaling = adjustSpeed 2 $ syncAll
  [ defineAnimation $ proc () ->
    annotate' animation -< g_ [transform_ $ translate x y <> " " <> scale 0.5 0.5]
  | x <- [0,160]
  , y <- [0,90]
  | animation <- [sinewave, morph_wave, highlight, progressMeters]]
