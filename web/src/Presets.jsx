const latex_draw =
`animation :: Animation
animation =
    bg \`sim\` (autoReverse $ drawText \`andThen\` fillText)
  where
    bg = mkAnimation 0 $ emit (mkBackground "black")
    msg = "\\\\sum_{k=1}^\\\\infty {1 \\\\over k^2} = {\\\\pi^2 \\\\over 6}"
    glyphs = center $ latexAlign msg
    fillText = mkAnimation 1 $ do
      s <- signal 0 1
      emit $ scale 5 $ withFillColor "white" $ withFillOpacity s glyphs
    drawText = mkAnimation 2 $ do
      s <- signal 0 1
      emit $ scale 5 $
        withStrokeColor "white" $ withFillOpacity 0 $ withStrokeWidth (Num 0.1) $
          partialSvg s glyphs`;

const bbox =
`animation :: Animation
animation = bg \`sim\`
    mapA (translate (-50) 0) bbox1 \`sim\`
    mapA (translate 50 0) bbox2
  where
    bg = mkAnimation 0 $ emit $ mkBackground "black"

bbox1 :: Animation
bbox1 = mkAnimation 5 $ do
    s <- signal 0 1
    emit $ mkGroup
      [ mkBoundingBox $ rotate (360*s) svg
      , withFillColor "white" $ rotate (360*s) svg ]
  where
    svg = scale 3 $ center $ latexAlign "\\\\sum_{k=1}^\\\\infty"

bbox2 :: Animation
bbox2 = autoReverse $ mkAnimation 2.5 $ do
  s <- signal 0 1
  emit $ mkGroup
    [ mkBoundingBox $ partialSvg s heartShape
    , withStrokeColor "white" $ withFillOpacity 0 $ partialSvg s heartShape ]

mkBoundingBox :: Tree -> Tree
mkBoundingBox svg = withStrokeColor "red" $ withFillOpacity 0 $
    mkRect (S.Num x, S.Num y) (S.Num w) (S.Num h)
  where
    (x, y, w, h) = boundingBox svg

heartShape =
    center $ rotateAroundCenter 225 $ mkPathString
      "M0.0,40.0 v-40.0 h40.0\\
      \\a20.0 20.0 90.0 0 1 0.0,40.0\\
      \\a20.0 20.0 90.0 0 1 -40.0,0.0 Z"`;

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
`animation :: Animation
animation = autoReverse $ mkAnimation 2.5 $ do
    morph <- signal 0 1
    emit $ mkBackground "black"
    emit $ withStrokeColor "white" $ translate (-320/2) (-180/2) $ mkGroup
      [ translate 30 50  $ mkLinePath wave1
      , translate 30 130 $ mkLinePath wave2
      , translate 30 90  $ mkLinePath $ morphPath wave1 wave2 morph
      , mkLine (Num 30, Num 10) (Num 30, Num 170)
      , mkLine (Num 30, Num 90) (Num 290, Num 90) ]
  where
    freq = 3; width = 260
    wave1 = approxFnData 100 $ \\idx -> (idx*width, sin (idx*pi*2*freq) * 20)
    wave2 = approxFnData 100 $ \\idx -> (idx*width, sin (idx*pi*2*(freq*3)) * 20)`;

const morph_wave_circle =
`animation :: Animation
animation = autoReverse $ mkAnimation 2.5 $ do
    idx <- signal 0 1
    emit $ mkBackground "black"
    emit $ withStrokeColor "white" $ translate (-320/2) (-180/2) $ mkGroup
      [ translate 30 90 $ mkLinePath $ morphPath circle wave1 idx
      , mkLine (Num 30, Num 10) (Num 30, Num 170)
      , mkLine (Num 30, Num 90) (Num 290, Num 90) ]
  where
    freq = 5; width = 260; radius = 50
    wave1 = approxFnData 100 $ \\idx -> (idx*width, sin (idx*pi*2*freq) * 20)
    circle = approxFnData 100 $ \\idx ->
      (cos (idx*pi*2+pi/2)*radius + width/2, sin (idx*pi*2+pi/2)*radius)`;

const progressMeters =
`animation :: Animation
animation =
    bg \`sim\` labels \`sim\`
    mapA (translate (-100) 0)  (adjustSpeed 1.0 progressMeter) \`simLoop\`
    mapA (translate 0 0) (adjustSpeed 2.0 progressMeter) \`simLoop\`
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
  h <- signal 0 100
  emit $ center $ mkGroup
    [ withStrokeColor "white" $ withStrokeWidth (Num 2) $ withFillOpacity 0 $
        mkRect (Num 0, Num 0) (Num 30) (Num 100)
    , withFillColor "white" $
        mkRect (Num 0, Num 0) (Num 30) (Num h) ]`

const latex_basic =
`animation :: Animation
animation = autoReverse $ mkAnimation 2 $ do
    s <- signal 0 1
    emit $ mkGroup
      [ mkBackground "black"
      , withStrokeColor "white" $ withFillOpacity 0 $ withStrokeWidth (Num 0.1) text
      , withFillColor "white" $ withFillOpacity s text ]
  where
    text = scale 4 $ center $ latexAlign
      "\\\\sum_{k=1}^\\\\infty {1 \\\\over k^2} = {\\\\pi^2 \\\\over 6}"`

const latex_color =
`animation :: Animation
animation = mkAnimation 1 $ do
    emit $ mkBackground "black"
    emit $ withStrokeWidth (Num 0.2) $
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
    // , {name: "Sinewave", code: sinewave }
    , {name: "Morphwave", code: morph_wave }
    , {name: "Morphwave Circle", code: morph_wave_circle }
    , {name: "Progress meters", code: progressMeters }
    // , {name: "Highlight", code: highlight }
    ]
  },
];
