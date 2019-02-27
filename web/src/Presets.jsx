const latex_draw =
`animation = pauseAtEnd 1 $ proc () -> do
    emit -< toHtml $ mkBackground "black"
    drawText msg \`andThen\` fillText msg -< ()
  where
    msg = "\\\\sum_{k=1}^\\\\infty {1 \\\\over k^2} = {\\\\pi^2 \\\\over 6}"
    placement = translate (320/2) (180/2) . scale 5
    fillText txt = proc () -> do
      duration 1 -< ()
      s <- signal 0 1 -< ()
      emit -< toHtml $ placement $
        withFillColor "white" $ withFillOpacity s $
          center $ latexAlign txt
    drawText txt = proc () -> do
      duration 2 -< ()
      s <- signal 0 1 -< ()
      emit -< toHtml $ placement $
        withStrokeColor "white" $ withFillOpacity 0 $ withStrokeWidth (Num 0.1) $
          partialSvg s $ center $ latexAlign txt`;

const bbox =
`animation :: Ani ()
animation = proc () -> do
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
    svg = scale 3 $ center $ latexAlign "\\\\sum_{k=1}^\\\\infty"

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
      \a20.0 20.0 90.0 0 1 -40.0,0.0 Z"`;

const sinewave =
`animation :: Ani ()
animation = proc () -> do
    duration 10 -< ()
    emit -< toHtml $ mkBackground "black"
    idx <- signalOscillate 0 1 -< ()
    emit -< do
      defs_ $ clipPath_ [id_ "clip"] $ toHtml $
        mkRect (Num 0, Num (-height)) (Num $ idx*width) (Num 320)
      toHtml $ translate margin height $ withStrokeColor "white" $
        withClipPathRef (Ref "clip") $ mkPathText $ renderPathText $ approxFnData 100 wave
      toHtml $ withStrokeColor "white" $
        mkLine (Num margin, Num 10) (Num margin, Num 170)
      toHtml $ withStrokeColor "white" $
        mkLine (Num margin, Num height) (Num (margin+width), Num height)
    let (circX, circY) = wave idx
    emit -< g_ [transform_ $ Lucid.translate margin height] $
      circle_ [num_ cx_ circX, num_ cy_ circY, r_ "3", fill_ "red"]
  where
    freq = 3; margin = 30; width = 260; height = 90
    wave idx = (idx*width, sin (idx*pi*2*freq) * 50)`;

const morph_wave =
`animation :: Ani ()
animation = proc () -> do
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
    wave1 = approxFnData 100 $ \\idx -> (idx*width, sin (idx*pi*2*freq) * 20)
    wave2 = approxFnData 100 $ \\idx -> (idx*width, sin (idx*pi*2*(freq*3)) * 20)`;

const morph_wave_circle =
`animation :: Ani ()
animation = proc t -> do
    duration 5 -< ()
    idx <- signalOscillate 0 1 -< ()
    emit -< toHtml $ mkBackground "black"
    emit -< toHtml $ withStrokeColor "white" $ mkGroup
      [ translate 30 90 $ mkPathText $ renderPathText $ morphPath circle wave1 idx
      , mkLine (Num 30, Num 10) (Num 30, Num 170)
      , mkLine (Num 30, Num 90) (Num 290, Num 90) ]
  where
    freq = 5; width = 260; radius = 50
    wave1 = approxFnData 100 $ \\idx -> (idx*width, sin (idx*pi*2*freq) * 20)
    circle = approxFnData 100 $ \\idx ->
      (cos (idx*pi*2+pi/2)*radius + width/2, sin (idx*pi*2+pi/2)*radius)`;

const progressMeters =
`animation :: Ani ()
animation = proc () -> do
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
  returnA -< ()`

const highlight =
`animation :: Ani ()
animation = proc () -> do
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
    b = 7; margin = 30; w = 30; h = 30
    commonAttrs c = [stroke_width_ "2", stroke_ c, fill_ c]
    highlightAttrs c = [stroke_width_ "2", stroke_ c, fill_opacity_ "0"]`;

const latex_basic =
`animation :: Ani ()
animation = proc () -> do
  duration 2 -< ()
  s <- signalOscillate 0 1 -< ()
  emit -< toHtml $ mkGroup
    [ mkBackground "black"
    , translate (320/2) (180/2) $ mkGroup
      [ withStrokeColor "white" $ withFillOpacity 0 $ withStrokeWidth (Num 0.1) text
      , withFillColor "white" $ withFillOpacity s text] ]
  where
    text = scale 4 $ center $ latexAlign
      "\\\\sum_{k=1}^\\\\infty {1 \\\\over k^2} = {\\\\pi^2 \\\\over 6}"`

const latex_color =
`animation :: Ani ()
animation = proc () -> do
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
    svg = scale 10 $ center $ latex "\\\\LaTeX"`


export default [
  { name: "Examples"
  , programs:
    [ {name: "LaTeX Draw", code: latex_draw }
    , {name: "LaTeX Color", code: latex_color }
    , {name: "LaTeX Basic", code: latex_basic }
    , {name: "Bounding boxes", code: bbox }
    , {name: "Sinewave", code: sinewave }
    , {name: "Morphwave", code: morph_wave }
    , {name: "Morphwave Circle", code: morph_wave_circle }
    , {name: "Progress meters", code: progressMeters }
    // , {name: "Highlight", code: highlight }
    ]
  },
];
