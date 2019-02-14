# reanimate

Reanimate is a reactive framework for creating non-interactive animations from SVG images.
This package consists of a set of arrow combinators, a renderer (using ffmpeg), and a Gtk-based
previewer.

The example gifs are rendered at 25 fps.

# Examples

## Sine wave

```haskell
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
```
![Sine wave](gifs/sinewave.gif)


## Morphing wave

```haskell
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

![Valentine's Day](gifs/valentine.webm)
