{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE ParallelListComp      #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies          #-}
module Reanimate.Examples where

import           Control.Lens                  ()
import qualified Data.Map                      as M
import           Graphics.SvgTree              as S hiding (circle, width)
import           Linear.V2

import           Reanimate.Combinators
import           Reanimate.Diagrams
import           Reanimate.LaTeX
import           Reanimate.Monad
import           Reanimate.Signal
import           Reanimate.Svg

import           Diagrams.Prelude              (deg, turn, withEnvelope, (@@))
import qualified Diagrams.Prelude              as D
import qualified Diagrams.TwoD.Path.LSystem    as D

{-
sinewave :: Ani ()
sinewave = proc () -> do
    duration 10 -< ()
    emit -< toHtml $ mkBackground "black"
    idx <- signalOscillate 0 1 -< ()
    emit -< do
      defs_ $ clipPath_ [id_ "clip"] $ toHtml $
        mkRect (Num 0, Num (-height)) (Num $ idx*width) (Num 320)
      toHtml $ translate margin height $ withStrokeColor "white" $
        withClipPathRef (Ref "clip") $ mkPathText $ renderPathText $ approxFnData 1000 wave
      toHtml $ withStrokeColor "white" $
        mkLine (Num margin, Num 10) (Num margin, Num 170)
      toHtml $ withStrokeColor "white" $
        mkLine (Num margin, Num height) (Num (margin+width), Num height)
    let (circX, circY) = wave idx
    emit -< g_ [transform_ $ Lucid.translate margin height] $
      circle_ [num_ cx_ circX, num_ cy_ circY, r_ "3", fill_ "red"]
  where
    freq = 3; margin = 30; width = 260; height = 90
    wave idx = (idx*width, sin (idx*pi*2*freq) * 50)

morph_wave :: Ani ()
morph_wave = proc () -> do
    duration 5 -< ()
    morph <- signalOscillate 0 1 -< ()
    emit -< toHtml $ mkBackground "black"
    emit -< toHtml $ withStrokeColor "white" $ mkGroup
      [ translate 30 50  $ mkPathText $ renderPathText wave1
      , translate 30 130 $ mkPathText $ renderPathText wave2
      , translate 30 90  $ mkPathText $ renderPathText $ morphPath wave1 wave2 morph
      , mkLine (Num 30, Num 10) (Num 30, Num 170)
      , mkLine (Num 30, Num 90) (Num 290, Num 90) ]
  where
    freq = 3; width = 260
    wave1 = approxFnData 1000 $ \idx -> (idx*width, sin (idx*pi*2*freq) * 20)
    wave2 = approxFnData 1000 $ \idx -> (idx*width, sin (idx*pi*2*(freq*3)) * 20)

morph_wave_circle :: Ani ()
morph_wave_circle = proc t -> do
    duration 5 -< ()
    idx <- signalOscillate 0 1 -< ()
    emit -< toHtml $ withStrokeColor "white" $ mkGroup
      [ mkBackground "black"
      , translate 30 90 $ mkPathText $ renderPathText $ morphPath circle wave1 idx
      , mkLine (Num 30, Num 10) (Num 30, Num 170)
      , mkLine (Num 30, Num 90) (Num 290, Num 90) ]
  where
    freq = 5; width = 260; radius = 50
    wave1 = approxFnData 1000 $ \idx -> (idx*width, sin (idx*pi*2*freq) * 20)
    circle = approxFnData 1000 $ \idx ->
      (cos (idx*pi*2+pi/2)*radius + width/2, sin (idx*pi*2+pi/2)*radius)

progressMeters :: Ani ()
progressMeters = proc () -> do
  emit -< rect_ [width_ "100%", height_ "100%", fill_ "black"]
  annotate' (adjustSpeed 1.0 progressMeter) -< g_ [transform_ $ Lucid.translate 40 20]
  annotate' (adjustSpeed 2.0 progressMeter) -< g_ [transform_ $ Lucid.translate 140 20]
  annotate' (adjustSpeed 0.5 progressMeter) -< g_ [transform_ $ Lucid.translate 240 20]

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
progressMeter = loop $ proc () -> do
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
    mkTransition from to = pauseAtEnd 1 $ proc () -> do
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
  emit -< toHtml $ mkBackground "black"
  annotate' $ follow
    [ sim
      [ sim [ paintStatic prev | prev <- [max 0 (n-4) .. n-1] ]
      , sim [ runAni "black" i | i <- [n-4], i>=0 ]
      , runAni "white" n ]
    | n <- [0..15]
    ] -< g_ [transform_ $ Lucid.translate (320/2) (180/2)]
  where
    paintStatic nth = proc () ->
      emit -< toHtml $ withStrokeColor "white" $
        square (20+nth*10)
    runAni color nth = circle_clip $ proc () -> do
      duration 1 -< ()
      emit -< toHtml $ withStrokeColor color $
        square (20+nth*10)
    square side = center $ withFillOpacity 0 $ withStrokeWidth (Num 2) $
      mkRect (Num 0, Num 0) (Num side) (Num side)

circle_clip :: Ani () -> Ani ()
circle_clip sub = proc () -> do
    arc <- signal (pi*2) 0 -< ()
    let startX = pack$show$sin 0 * 1000
        startY = pack$show$cos 0 * 1000
        xPos = pack$show$sin arc * 1000
        yPos = pack$show$cos arc * 1000
        long = if arc < pi then "1" else "0"
    emit -< clipPath_ [id_ $ uniqName] $
      path_ [ d_ $ "M "<>startX<>" "<>startY<>" A 1000 1000 0 "<>long<>" 1 "
                  <>xPos<> " "<>yPos<>" L 0 0 Z"]
    annotate' sub -<
      g_ [clip_path_ $ "url(#"<>uniqName<>")"]
  where
    uniqName = "clip" -- XXX: Not very unique?


scaling :: Ani ()
scaling = adjustSpeed 2 $ syncAll
  [ proc () ->
    annotate' animation -< g_ [transform_ $ Lucid.translate x y <> " " <> Lucid.scale 0.5 0.5]
  | x <- [0,160]
  , y <- [0,90]
  | animation <- [sinewave, morph_wave, highlight, progressMeters]]


label :: String -> Ani ()
label str = proc () -> do
  emit -< text_ [x_ "0", y_ "16", font_size_ "16"
        , fill_ "white"] (toHtml str)

valentine :: Ani ()
valentine = proc () -> do
    follow
     [ all_red
     , sim [ background
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
           , follow [heart_ani, heart_disappear]
           , follow [backgroundDelay, message "", message ""
                    , message "", message "爱", message ""
                    , message "", message ""]]
     ] -<()
  where
    all_red = proc () -> do
      duration 1 -< ()
      emit -< rect_ [width_ "100%", height_ "100%", fill_ "red"]
    background = freezeAtEnd $ proc () -> do
      duration 2 -< ()
      n <- signal 0 0xFF -< ()
      let color = "#FF" ++ hex n ++ hex n
      emit -< rect_ [width_ "100%", height_ "100%", fill_ $ pack color]
    backgroundDelay = freezeAtEnd $ proc () -> do
      duration (animationDuration background-1) -< ()
      returnA -< ()
    heart_ani = repeatAni 10 $ proc () -> do
      duration 1 -< ()
      n <- signalOscillateSCurve 2 0.9 1.1 -< ()
      annotate' drawHeart -< g_ [transform_ $ Lucid.translate 160 110] . g_ [transform_ $ Lucid.scale n n <> " "]
    heart_disappear = proc () -> do
      duration 3 -< ()
      n  <- signal 0.9 10 -< ()
      annotate' drawHeart -< g_ [transform_ $ Lucid.translate 160 110] . g_ [transform_ $ Lucid.scale n n <> " "]
    white = loop $ proc () -> do
      duration 1 -< ()
      emit -< rect_ [width_ "100%", height_ "100%", fill_ "#FFFFFF"]
    fallingLove xPos = proc () -> do
      duration 2 -< ()
      n <- signal 0 1 -< ()
      o <- signalOscillate (-1) 1 -< ()
      emit -<
        g_ [transform_ $ Lucid.translate (xPos*360) (210*n)] $
          g_ [transform_ $ Lucid.rotate (45*o)] $
            text_ [font_size_ "18"
                  ,text_anchor_ "middle"
                  ,fill_ "red"] "爱"
    message txt = proc () -> do
      duration 1 -< ()
      o <- signalOscillate 0 1 -< ()
      n <- signalOscillateSCurve 2 0.9 1.1 -< ()
      emit -<
        g_ [transform_ $ Lucid.translate 160 110, num_ opacity_ o] $
        g_ [transform_ $ Lucid.scale n n ] $
          text_ [x_ "0", y_ "-12", font_size_ "24"
                    , text_anchor_ "middle"
                    , fill_ "white"] txt

    drawHeart = proc () -> do
      emit -<
        g_ [transform_ $ Lucid.translate (-170) (-260)] $
          g_ [transform_ $ Lucid.rotateAround 225 150 121 <> " " <> Lucid.scale 0.4 0.4] $
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
    drawLine = freezeAtEnd $ proc _ -> do
      label "drawLine" -< ()
      duration 1 -< ()
      n <- signal margin (width+margin) -< ()
      emit -< do
        line_ [ num_ x1_ margin, num_ y1_ height
              , num_ x2_ n,      num_ y2_ height
              , stroke_ "white"]
        circle_ [num_ cx_ n, num_ cy_ height, r_ "3", fill_ "red"]
    drawFirstWave = freezeAtEnd $ proc move -> do
      label "drawFirstWave" -< ()
      duration 3 -< ()
      n <- signal 0 1 -< ()
      emit -< do
        g_ [transform_ $ Lucid.translate margin height] $ renderPath $ morphPath line1 (wave1 move) n
        let circleY = sum [ sin ((1+move)*pi*2*freq) * 20 | freq <- freqs ]
        circle_ [num_ cx_ (width+margin), num_ cy_ (height+circleY*n), num_ r_ 3, fill_ "red"]
    drawSecondWave = freezeAtEnd $ proc move -> do
      label "drawSecondWave" -< ()
      duration 3 -< ()
      emit -< do
        g_ [transform_ $ Lucid.translate margin height] $ renderPath $ wave1 move
        let circleY = sum [ sin ((1+move)*pi*2*freq) * 20 | freq <- freqs ]
        circle_ [num_ cx_ (width+margin), num_ cy_ (height+circleY), num_ r_ 3, fill_ "red"]
    drawUpWave = freezeAtEnd $ proc move -> do
      label "drawUpWave" -< ()
      duration 2 -< ()
      n <- signal 0 1 -< ()
      emit -< do
        g_ [transform_ $ Lucid.scale 1 (1-0.5*n)] $ do
          g_ [transform_ $ Lucid.translate margin height] $ renderPath $ wave1 move
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
  emit -< toHtml $ mkGroup
    [ mkBackground "black"
    , translate (320/2) (180/2) $ mkGroup
      [ withStrokeColor "white" $ withFillOpacity 0 $ withStrokeWidth (Num 0.1) text
      , withFillColor "white" $ withFillOpacity s text] ]
  where
    text = scale 4 $ center $ latexAlign
      "\\sum_{k=1}^\\infty {1 \\over k^2} = {\\pi^2 \\over 6}"

bezier :: Ani ()
bezier = adjustSpeed 0.4 $ proc () -> do
  emit -< rect_ [width_ "100%", height_ "100%", fill_ "black"]
  follow
    [ orderN [pointA, pointB]
    , morph [pointA, pointA, pointB] [pointA, pointC, pointB]
    , orderN [pointA, pointC, pointB]
    , morph [pointA, pointC, pointB, pointB] [pointA, pointC, pointD, pointB]
    , orderN [pointA, pointC, pointD, pointB]
    , morph [pointA, pointC, pointD, pointB] [pointA, pointA, pointB, pointB]] -< ()
  where
    pointA = (70,130); pointB = (270,120); pointC = (30,30); pointD = (250,50)

    morph old new = proc () -> do
      duration 0.5 -< ()
      s <- signal 0 1 -< ()
      let new' = map (\(a,b) -> between a b s) (zip old new)
      emit -< forM_ (zip new' (tail new')) $ \(a,b) -> do
        renderPath $
          approxFnData 100 $ \idx ->
            between a b idx
      emit -< mapM_ secondaryCircleAt new'
      emit -< primaryCircleAt (head new')
    orderN lst = proc () -> do
      duration 2 -< ()
      s <- signalOscillate 0 1 -< ()
      emit -< primaryCircleAt =<< orderN' (map const lst) s <* mapM_ secondaryCircleAt lst
    orderN' [a] s = do
      renderPath $ take (round $ 100*s) $ approxFnData 100 $ \idx -> a idx
      return (a s)
    orderN' lst s = do
      forM_ (zip lst (tail lst)) $ \(a,b) -> renderPath $
          approxFnData 100 $ \idx ->
            between (a s) (b s) idx
      let middlePoints = map (\(a,b) -> \idx -> between (a idx) (b idx) idx) (zip lst (tail lst))
      orderN' middlePoints s <* mapM_ secondaryCircleAt (map ($s) middlePoints)

    secondaryCircleAt (x,y) = circle_ [num_ cx_ x, num_ cy_ y, num_ r_ 3, fill_ "green"]
    primaryCircleAt (x,y) = circle_ [num_ cx_ x, num_ cy_ y, num_ r_ 3, fill_ "red"]
    between a b _ | a==b = a
    between (x1, y1) (x2, y2) idx =
      ( x1 + idx * (x2 - x1)
      , y1 + idx * (x2-x1) * (y2 - y1) / (x2 - x1))

pathSquare :: Ani ()
pathSquare = proc () -> do
    duration 2 -< ()
    s <- signalOscillate 0 1 -< ()
    emit -< rect_ [width_ "100%", height_ "100%", fill_ "black"]
    emit -< g_ [stroke_ "white"] $ toHtml (square s)
  where
    square s = S.PathTree (myPath s)
    myPath s = S.defaultSvg
      & S.pathDefinition .~ interpolatePathCommands s myPathCmds
    myPathCmds =
      [ S.MoveTo S.OriginAbsolute [V2 100 100]
      , S.LineTo S.OriginAbsolute [V2 200 150]
      , S.LineTo S.OriginRelative [V2 (-10) (-100)]
      , S.EndPath
      ]

latex_draw :: Ani ()
latex_draw = pauseAtEnd 1 $ proc () -> do
  emit -< toHtml $ mkBackground "black"
  drawText `andThen` fillText -< ()
  where
    msg = "\\sum_{k=1}^\\infty {1 \\over k^2} = {\\pi^2 \\over 6}"
    glyphs = center $ latexAlign msg
    placement = translate (320/2) (180/2) . scale 5
    fillText = proc () -> do
      duration 1 -< ()
      s <- signal 0 1 -< ()
      emit -< toHtml $ placement $
          withFillColor "white" $ withFillOpacity s $
            glyphs
    drawText = proc () -> do
      duration 2 -< ()
      s <- signal 0 1 -< ()
      emit -< toHtml $ placement $
        withStrokeColor "white" $ withFillOpacity 0 $ withStrokeWidth (Num 0.1) $
          partialSvg s glyphs


bbox :: Ani ()
bbox = proc () -> do
  emit -< toHtml $ mkBackground "black"
  duration 5 -< ()
  annotate' bbox1 -< g_ [transform_ $ Lucid.translate (320/2-50) (180/2)]
  annotate' bbox2 -< g_ [transform_ $ Lucid.translate (320/2+50) (180/2)]

bbox1 :: Ani ()
bbox1 = proc () -> do
  s <- signal 0 1 -< ()
  emit -< do
    toHtml $ mkBoundingBox $ rotate (360*s) svg
    toHtml $ withFillColor "white" $ rotate (360*s) svg
  where
    svg = scale 3 $ center $ latexAlign "\\sum_{k=1}^\\infty"

bbox2 :: Ani ()
bbox2 = proc () -> do
  s <- signalOscillate 0 1 -< ()
  emit -< do
    toHtml $ mkBoundingBox $ partialSvg s heartShape
    toHtml $ withStrokeColor "white" $ withFillOpacity 0 $ partialSvg s heartShape

mkBoundingBox :: Tree -> Tree
mkBoundingBox svg = withStrokeColor "red" $ withFillOpacity 0 $
    mkRect (S.Num x, S.Num y) (S.Num w) (S.Num h)
  where
    (x, y, w, h) = boundingBox svg

heartShape =
    center $ rotateAroundCenter 225 $ mkPathString
      "M0.0,40.0 v-40.0 h40.0\
      \a20.0 20.0 90.0 0 1 0.0,40.0\
      \a20.0 20.0 90.0 0 1 -40.0,0.0 Z"

latex_color :: Ani ()
latex_color = proc () -> do
    duration 0.1 -< ()
    emit -< toHtml $ mkBackground "black"
    emit -< toHtml $ translate (320/2) (180/2) $ withStrokeWidth (Num 0.2) $
      withStrokeColor "white" $
      withSubglyphs [0] (withFillColor "blue") $
      withSubglyphs [1] (withFillColor "yellow") $
      withSubglyphs [2] (withFillColor "green") $
      withSubglyphs [3] (withFillColor "red") $
      withSubglyphs [4] (withFillColor "darkslategrey") $
      svg
  where
    svg = scale 10 $ center $ latex "\\LaTeX"
-}


morph_wave :: Animation
morph_wave = autoReverse $ mkAnimation 2.5 $ do
    morph <- getSignal signalLinear
    emit $ mkBackground "black"
    emit $ withStrokeColor "white" $ translate (-320/2) (-180/2) $ mkGroup
      [ translate 30 50  $ mkLinePath wave1
      , translate 30 130 $ mkLinePath wave2
      , translate 30 90  $ mkLinePath $ morphPath wave1 wave2 morph
      , mkLine (Num 30, Num 10) (Num 30, Num 170)
      , mkLine (Num 30, Num 90) (Num 290, Num 90) ]
  where
    freq = 3; width = 260
    wave1 = approxFnData 100 $ \idx -> (idx*width, sin (idx*pi*2*freq) * 20)
    wave2 = approxFnData 100 $ \idx -> (idx*width, sin (idx*pi*2*(freq*3)) * 20)

morph_wave_circle :: Animation
morph_wave_circle = autoReverse $ mkAnimation 2.5 $ do
    idx <- getSignal signalLinear
    emit $ mkBackground "black"
    emit $ withStrokeColor "white" $ translate (-320/2) (-180/2) $ mkGroup
      [ translate 30 90 $ mkLinePath $ morphPath circle wave1 idx
      , mkLine (Num 30, Num 10) (Num 30, Num 170)
      , mkLine (Num 30, Num 90) (Num 290, Num 90) ]
  where
    freq = 5; width = 260; radius = 50
    wave1 = approxFnData 100 $ \idx -> (idx*width, sin (idx*pi*2*freq) * 20)
    circle = approxFnData 100 $ \idx ->
      (cos (idx*pi*2+pi/2)*radius + width/2, sin (idx*pi*2+pi/2)*radius)

progressMeters :: Animation
progressMeters =
    bg `sim` labels `sim`
    mapA (translate (-100) 0)  (adjustSpeed 1.0 progressMeter) `simLoop`
    mapA (translate 0 0) (adjustSpeed 2.0 progressMeter) `simLoop`
    mapA (translate 100 0) (adjustSpeed 0.5 progressMeter)
  where
    bg = mkAnimation 0 $ emit $ mkBackground "black"
    labels = mkAnimation 0 $ emit $ translate 0 70 $ withFillColor "white" $ mkGroup
      [ translate (-100) 0 $ scale 2 $ center $ latex "1x"
      , translate 0 0      $ scale 2 $ center $ latex "2x"
      , translate 100 0    $ scale 2 $ center $ latex "0.5x"
      ]

progressMeter :: Animation
progressMeter = mkAnimation 3 $ do
  h <- getSignal $ signalFromTo 0 100 signalLinear
  emit $ center $ mkGroup
    [ withStrokeColor "white" $ withStrokeWidth (Num 2) $ withFillOpacity 0 $
        mkRect (Num 30) (Num 100)
    , withFillColor "white" $
        mkRect (Num 30) (Num h) ]


diaSize :: Animation
diaSize = mkAnimation 0.1 $ do
    emit $ mkBackground "white"
    emit $ translate (-320/2) (-180/2) dSvg
  where
    dSvg = renderDiagram $ withEnvelope (D.rect 320 180 :: SvgDiagram) $
      D.scale 3 $
      D.translate (V2 0 (-30)) $
      D.rotate (90 @@ deg) $
      D.lwO 0.1 $ D.strokePath (D.getTurtlePath (D.tree3 4))

wavyTree :: Animation
wavyTree = mkAnimation 1 $ do
    s <- oscillate $ getSignal $ signalFromTo 1 2 signalLinear
    emit $ mkBackground "white"
    emit $ translate (-320/2) (-180/2) (dSvg s)
  where
    dSvg s = renderDiagram $ withEnvelope (D.rect 320 180 :: SvgDiagram) $
      D.scale 3 $
      D.translate (V2 0 (-30)) $
      D.rotate (90 @@ deg) $
      D.lwO 0.1 $ D.strokePath (D.getTurtlePath (tree s))
    gens = 4
    tree s =
      D.lSystem gens (s/16 @@ turn) (D.symbols "F") rules
    rules = M.fromList [D.rule 'F' "FF-[->F+F+>F]+[+>F->F->F]"]
