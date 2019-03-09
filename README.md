# reanimate

Reanimate is a reactive framework for creating non-interactive animations from SVG images.
This package consists of a set of arrow combinators, a renderer (using ffmpeg), and a web-based
previewer. Inline latex code is supported when 'latex' and 'dvisvgm' are installed.

Nothing about the API is stable at this point.

Live coding playground: https://lemmih.github.io/reanimate/

# TODO

* ~~website for live coding~~
* ~~bounding boxes~~
* ~~Use svg-tree instead of lucid-svg~~
* Improve bounding box approximations
* Select desired FPS in web previewer
* Download rendered animations in web previewer
* alignment and positioning combinators
* Use FSEvents to watch for file changes on mac.


# Examples

The example gifs are displayed at 25 fps. The source code for many of these examples are available
on the live coding playground.

![Goo](gifs/goo.gif)
![Drawing LaTeX equations](gifs/latex_draw.gif)
![Bounding boxes](gifs/bbox.gif)
![Colorful LaTeX](gifs/latex_color.gif)
![Bezier curves](gifs/bezier.gif)
![Sine wave](gifs/sinewave.gif)
![Morphing wave](gifs/morphwave.gif)
![Morphing wave to circle](gifs/morphwave_circle.gif)
![Speed modification](gifs/progress.gif)
![Highlight](gifs/highlight.gif)
![Clipping](gifs/clip_rect.gif)
![Scaling](gifs/scaling.gif)
![Valentine's Day](gifs/valentine.gif)
![Basic LaTeX](gifs/latex_basic.gif)
