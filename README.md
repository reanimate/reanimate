# reanimate

# Examples

![Sine wave](gifs/sinewave.gif)

![Morphing wave](gifs/morphwave.gif)

![Morphing wave to circle](gifs/morphwave_circle.gif)

![Speed modification](gifs/progress.gif)

![Highlight](gifs/highlight.gif)

![Clipping](gifs/clip_rect.gif)


## scaling

Animations can be reused, scaled and stretched in time. This example
reuse four different animations of different lengths, scales their runtime
so they're all in sync, and increases the playback speed.

```
scaling :: Ani ()
scaling = adjustSpeed 2 $ defineAnimation $ syncAll
  [ proc () ->
    annotate' (defineAnimation animation)
      -< g_ [transform_ $ translate x y] . g_ [transform_ $ scale 0.5 0.5]
  | x <- [0,160]
  , y <- [0,90]
  | animation <- [sinewave, morph_wave, highlight, progressMeters]
  ]
```
![Scaling](gifs/scaling.gif)
