# reanimate

Reanimate is a reactive framework for creating non-interactive animations from SVG images.
This package consists of a set of arrow combinators, a renderer (using ffmpeg), and a Gtk-based
previewer. Inline latex code is supported when 'latex' and 'dvisvgm' are installed.

Nothing about the API is stable at this point.

The example gifs are displayed at 25 fps.

# TODO

* bounding boxes
* alignment and positioning combinators
* automatic code reloading in the Gtk viewer
* website for live coding

# Examples

## Drawing latex equations
```haskell
latex_draw :: Ani ()
latex_draw = pauseAtEnd 1 $ defineAnimation $ proc () -> do
  emit -< toHtml $ mkBackground "black"
  drawText msg `andThen` fillText msg -< ()
  where
    msg = "\\sum_{k=1}^\\infty {1 \\over k^2} = {\\pi^2 \\over 6}"
    placement = translate (320/2) (180/2) . scale 5
    fillText txt = defineAnimation $ proc () -> do
      duration 1 -< ()
      s <- signal 0 1 -< ()
      emit -< toHtml $ placement $
          withFillColor "white" $ withFillOpacity s $
            center $ latexAlign txt
    drawText txt = defineAnimation $ proc () -> do
      duration 2 -< ()
      s <- signal 0 1 -< ()
      emit -< toHtml $ placement $
        withStrokeColor "white" $ withFillOpacity 0 $ withStrokeWidth (Num 0.1) $
          partialSvg s $ center $ latexAlign txt
```
![Drawing LaTeX equations](gifs/latex_draw.gif)

## Bounding boxes

```haskell
bbox :: Ani ()
bbox = proc () -> do
  emit -< rect_ [width_ "100%", height_ "100%", fill_ "black"]
  annotate' bbox1 -< g_ [transform_ $ Lucid.translate (320/2-50) (180/2)]
  annotate' bbox2 -< g_ [transform_ $ Lucid.translate (320/2+50) (180/2)]

bbox1 :: Ani ()
bbox1 = defineAnimation $ proc () -> do
  duration 5 -< ()
  s <- signal 0 1 -< ()
  emit -< do
    toHtml $ mkBoundingBox $ rotate (360*s) svg
    toHtml $ withFillColor "white" $ rotate (360*s) svg
  where
    svg = scale 3 $ center $ latexAlign "\\sum_{k=1}^\\infty"

bbox2 :: Ani ()
bbox2 = defineAnimation $ proc () -> do
  duration 5 -< ()
  s <- signalOscillate 0 1 -< ()
  emit -< do
    toHtml $ mkBoundingBox $ partialSvg s heartShape
    toHtml $ withStrokeColor "white" $ withFillOpacity 0 $ partialSvg s heartShape
```
![Bounding boxes](gifs/bbox.gif)

## Colorful LaTeX
```haskell
latex_color :: Ani ()
latex_color = proc () -> do
    duration 0.1 -< ()
    emit -< toHtml $ mkBackground "black"
    emit -< toHtml $ translate (320/2) (180/2) $ withStrokeWidth (Num 0.2) $
      withStrokeColor "white" $
      withSubglyphs 0 1 (withFillColor "blue") $
      withSubglyphs 1 2 (withFillColor "yellow") $
      withSubglyphs 2 3 (withFillColor "green") $
      withSubglyphs 3 4 (withFillColor "red") $
      withSubglyphs 4 5 (withFillColor "darkslategrey") $
      svg
  where
    svg = scale 10 $ center $ latex "\\LaTeX"
```
![Colorful LaTeX](gifs/latex_color.gif)

## Bezier curves

![Bezier curves](gifs/bezier.gif)

## Sine wave

```haskell
sinewave :: Ani ()
sinewave = proc () -> do
    duration 10 -< ()
    emit -< toHtml $ mkBackground "black"
    idx <- signalOscillate 0 1 -< ()
    emit -< do
      defs_ $ clipPath_ [id_ "clip"] $ toHtml $
        mkRect (Num 0, Num (-height)) (Num $ idx*width) (Percent 100)
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
```
![Sine wave](gifs/sinewave.gif)


## Morphing wave

```haskell
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
```
![Morphing wave](gifs/morphwave.gif)


## Morph wave to circle

```haskell
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
```
![Morphing wave to circle](gifs/morphwave_circle.gif)

## Speed modification

```haskell
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
```
![Speed modification](gifs/progress.gif)

## Highlights
![Highlight](gifs/highlight.gif)

## Clipping

```haskell
clip_rect :: Ani ()
clip_rect = proc () -> do
  emit -< rect_ [width_ "100%", height_ "100%", fill_ "black"]
  follow
    [ sim
      [ sim [ paintStatic prev | prev <- [max 0 (n-4) .. n-1] ]
      , sim [ runAni "black" i | i <- [n-4], i>=0 ]
      , runAni "white" n ]
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
```
![Clipping](gifs/clip_rect.gif)


## scaling

Animations can be reused, scaled and stretched in time. This example
reuse four different animations of different lengths, scales their runtime
so they're all in sync, and increases the playback speed.

```haskell
scaling :: Ani ()
scaling = adjustSpeed 2 $ syncAll
  [ defineAnimation $ proc () ->
    annotate' animation -< g_ [transform_ $ translate x y <> " " <> scale 0.5 0.5]
  | x <- [0,160]
  , y <- [0,90]
  | animation <- [sinewave, morph_wave, highlight, progressMeters]]
```
![Scaling](gifs/scaling.gif)

## Valentine's Day

![Valentine's Day](gifs/valentine.gif)

## Basic LaTeX

```haskell
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
```
![Basic LaTeX](gifs/latex_basic.gif)
