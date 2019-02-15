{-# LANGUAGE OverloadedStrings, Arrows, ParallelListComp #-}
module Reanimate.Examples where

import Lucid.Svg
import Data.Text (Text, pack)
import Data.Monoid ((<>))
import Control.Arrow (returnA, (>>>))
import Control.Monad
import Text.Printf
import Numeric

import Reanimate.Arrow
import Reanimate.Combinators
import Reanimate.LaTeX

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

    let (circX, circY) = wave idx
    emit -< g_ [transform_ $ translate margin height] $
      circle_ [num_ cx_ circX, num_ cy_ circY, r_ "3", fill_ "red"]
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


label :: String -> Ani ()
label str = proc () -> do
  emit -< text_ [x_ "0", y_ "16", font_size_ "16"
        , fill_ "white"] (toHtml str)

heart :: Ani ()
heart = proc () -> do
    emit -< rect_ [width_ "100%", height_ "100%", fill_ "#FFFFFF"]
    -- duration 1 -< ()
    -- annotate' drawHeart -< g_ [transform_ $ scale 0.5 0.5 <> " " <> translate 200 200]
    emit -< rect_ [width_ "100%", height_ "100%", fill_ "black"]
    follow
     [ all_read
     , sim [ follow [background]
           , follow [backgroundDelay, sim [delay 6.4 (fallingLove 0.09)
                                          ,delay 4.9 (fallingLove 0.12)
                                          ,delay 4.5 (fallingLove 0.88)
                                          ,delay 0.3 (fallingLove 0.43)
                                          ,delay 5.3 (fallingLove 0.93)
                                          ,delay 0.1 (fallingLove 0.80)
                                          ,delay 1.1 (fallingLove 0.39)
                                          ,delay 2.3 (fallingLove 0.21)
                                          ,delay 2.9 (fallingLove 0.77)
                                          ,delay 3.4 (fallingLove 0.46)
                                          ,delay 6.2 (fallingLove 0.19)
                                          ,delay 5.9 (fallingLove 0.53)
                                          ,delay 3.2 (fallingLove 0.14)
                                          ,delay 7.7 (fallingLove 0.99) ]]
           , follow [heart_ani, sim [heart_disappear]]
           , follow [backgroundDelay, message "", message ""
                    , message "", message "爱", message ""
                    , message "", message ""]]
     ] -<()
  where
    all_read = defineAnimation $ proc () -> do
      duration 1 -< ()
      emit -< rect_ [width_ "100%", height_ "100%", fill_ "red"]
    background = defineAnimation $ proc () -> do
      duration 2 -< ()
      n <- signal 0 0xFF -< ()
      let color = "#FF" ++ hex n ++ hex n
      emit -< rect_ [width_ "100%", height_ "100%", fill_ $ pack color]
    rev_background = defineAnimation $ proc () -> do
      duration 2 -< ()
      n <- signal 0xFF 0 -< ()
      let color = "#FF" ++ hex n ++ hex n
      emit -< rect_ [width_ "100%", height_ "100%", fill_ $ pack color]
    backgroundDelay = defineAnimation $ proc () -> do
      duration (animationDuration background-1) -< ()
      returnA -< ()
    heart_ani = repeatAni 10 $ defineAnimation $ proc () -> do
      duration 1 -< ()
      n <- signalOscillateSCurve 2 0.9 1.1 -< ()
      annotate' drawHeart -< g_ [transform_ $ translate 160 110] . g_ [transform_ $ scale n n <> " "]
    heart_disappear = defineAnimation $ proc () -> do
      duration 3 -< ()
      n  <- signal 0.9 10 -< ()
      annotate' drawHeart -< g_ [transform_ $ translate 160 110] . g_ [transform_ $ scale n n <> " "]
    white = loop $ defineAnimation $ proc () -> do
      duration 1 -< ()
      emit -< rect_ [width_ "100%", height_ "100%", fill_ "#FFFFFF"]
    fallingLove xPos = defineAnimation $ proc () -> do
      duration 2 -< ()
      n <- signal 0 1 -< ()
      o <- signalOscillate (-1) 1 -< ()
      emit -<
        g_ [transform_ $ translate (xPos*360) (210*n)] $
          g_ [transform_ $ rotate (45*o)] $
            text_ [font_size_ "18"
                  ,text_anchor_ "middle"
                  ,fill_ "red"] "爱"
    message txt = defineAnimation $ proc () -> do
      duration 1 -< ()
      o <- signalOscillate 0 1 -< ()
      n <- signalOscillateSCurve 2 0.9 1.1 -< ()
      emit -<
        g_ [transform_ $ translate 160 110, num_ opacity_ o] $
        g_ [transform_ $ scale n n ] $
          text_ [x_ "0", y_ "-12", font_size_ "24"
                    , text_anchor_ "middle"
                    , fill_ "white"] txt

    drawHeart = proc () -> do
      emit -<
        g_ [transform_ $ translate (-170) (-260)] $
          g_ [transform_ $ rotateAround 225 150 121 <> " " <> scale 0.4 0.4] $
            path_ ([stroke_ "red", fill_"red", d_ dat])
    dat = "M0 200 v-200 h200      a100,100 90 0,1 0,200     a100,100 90 0,1 -200,0     z"
    hex n = if n < 0x10 then "0" ++ showHex (round n) ""
            else showHex (round n) ""

frequencies :: Ani ()
frequencies = proc () -> do
    emit -< rect_ [width_ "100%", height_ "100%", fill_ "black"]
    n <- signal 0 2 -< ()
    follow -- [drawUpWave
      [ drawLine
      , drawFirstWave
      , drawSecondWave
      , drawUpWave
      ] -< n
  where
    freqs = [11, 5, 17]; margin = 30; width = 260; height = 90
    drawLine = defineAnimation $ proc _ -> do
      label "drawLine" -< ()
      duration 1 -< ()
      n <- signal margin (width+margin) -< ()
      emit -< do
        line_ [ num_ x1_ margin, num_ y1_ height
              , num_ x2_ n,      num_ y2_ height
              , stroke_ "white"]
        circle_ [num_ cx_ n, num_ cy_ height, r_ "3", fill_ "red"]
    drawFirstWave = defineAnimation $ proc move -> do
      label "drawFirstWave" -< ()
      duration 3 -< ()
      n <- signal 0 1 -< ()
      emit -< do
        g_ [transform_ $ translate margin height] $ renderPath $ morphPath line1 (wave1 move) n
        let circleY = sum [ sin ((1+move)*pi*2*freq) * 20 | freq <- freqs ]
        circle_ [num_ cx_ (width+margin), num_ cy_ (height+circleY*n), num_ r_ 3, fill_ "red"]
    drawSecondWave = defineAnimation $ proc move -> do
      label "drawSecondWave" -< ()
      duration 3 -< ()
      emit -< do
        g_ [transform_ $ translate margin height] $ renderPath $ wave1 move
        let circleY = sum [ sin ((1+move)*pi*2*freq) * 20 | freq <- freqs ]
        circle_ [num_ cx_ (width+margin), num_ cy_ (height+circleY), num_ r_ 3, fill_ "red"]
    drawUpWave = defineAnimation $ proc move -> do
      label "drawUpWave" -< ()
      duration 2 -< ()
      n <- signal 0 1 -< ()
      emit -< do
        g_ [transform_ $ scale 1 (1-0.5*n)] $ do
          g_ [transform_ $ translate margin height] $ renderPath $ wave1 move
          let circleY = sum [ sin ((1+move)*pi*2*freq) * 20 | freq <- freqs ]
          circle_ [num_ cx_ (width+margin), num_ cy_ (height+circleY), num_ r_ 3, fill_ "red"]
    line1 = approxFnData 1000 $ \idx ->
      (idx*width, 0)
    wave1 n = approxFnData 1000 $ \idx ->
      (idx*width, sum [ sin ((idx+n)*pi*2*freq) * 20 | freq <- freqs ])


latex_basic :: Ani ()
latex_basic = proc () -> do
  duration 2 -< ()
  s <- signalOscillate 0 1 -< ()
  emit -< rect_ [width_ "100%", height_ "100%", fill_ "black"]
  emit -<
    g_ [transform_ $ translate 20 15 <> " " <> scale 4 4] $ do
      g_ [stroke_ "white", fill_opacity_ "0", stroke_width_ "0.1"] text
      g_ [fill_ "white", num_ fill_opacity_ s]                     text
  where
    text = latex "\\sum_{k=1}^\\infty {1 \\over k^2} = {\\pi^2 \\over 6}"

bezier :: Ani ()
bezier = adjustSpeed 0.5 $ proc () -> do
  emit -< rect_ [width_ "100%", height_ "100%", fill_ "black"]
  follow
    [orderN [pointA, pointB]
    ,morph [pointA, pointA, pointB] [pointA, pointC, pointB]
    ,orderN [pointA, pointC, pointB]
    ,morph [pointA, pointC, pointB, pointB] [pointA, pointC, pointD, pointB]
    ,orderN [pointA, pointC, pointD, pointB]
    ,morph [pointA, pointC, pointD, pointB] [pointA, pointA, pointB, pointB]] -< ()
  where
    pointA = (70,130); pointB = (270,120); pointC = (30,30); pointD = (250,50)

    morph old new = defineAnimation $ proc () -> do
      duration 0.5 -< ()
      s <- signal 0 1 -< ()
      let new' = map (\(a,b) -> between a b s) (zip old new)
      emit -< forM_ (zip new' (tail new')) $ \(a,b) -> do
        renderPath $
          approxFnData 1000 $ \idx ->
            between a b idx
      emit -< mapM_ circleAt new'
    orderN lst = defineAnimation $ proc () -> do
      duration 2 -< ()
      s <- signalOscillate 0 1 -< ()
      emit -< orderN' (map const lst) s
      emit -< mapM_ circleAt lst
    orderN' [a] s = do
      renderPath $ take (round $ 1000*s) $ approxFnData 1000 $ \idx -> a idx
      circle_ [num_ cx_ (fst (a s)), num_ cy_ (snd (a s)), num_ r_ 3, fill_ "red"]
    orderN' lst s = do
      forM_ (zip lst (tail lst)) $ \(a,b) -> renderPath $
          approxFnData 1000 $ \idx ->
            between (a s) (b s) idx
      let middlePoints = map (\(a,b) -> \idx -> between (a idx) (b idx) idx) (zip lst (tail lst))
      mapM_ (\p -> circleAt (p s)) middlePoints
      orderN' middlePoints s

    circleAt (x,y) = circle_ [num_ cx_ x, num_ cy_ y, num_ r_ 3, fill_ "green"]
    between a b _ | a==b = a
    between (x1, y1) (x2, y2) idx =
      ( x1 + idx * (x2 - x1)
      , y1 + idx * (x2-x1) * (y2 - y1) / (x2 - x1))
