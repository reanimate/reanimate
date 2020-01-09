[![Hackage](https://img.shields.io/hackage/v/reanimate.svg?color=success)](http://hackage.haskell.org/package/reanimate)
[![Build Status](https://dev.azure.com/lemmih0612/reanimate/_apis/build/status/Lemmih.reanimate?branchName=master)](https://dev.azure.com/lemmih0612/reanimate/_build/latest?definitionId=1&branchName=master)
[![Documentation Status](https://readthedocs.org/projects/reanimate/badge/?version=latest)](https://reanimate.readthedocs.io/en/latest/?badge=latest)

![Platforms](https://img.shields.io/badge/platform-linux%20%7C%20osx%20%7C%20windows-informational)

# reanimate

Reanimate is a library for programmatically generating animations with a twist towards
mathematics / 2D vector drawings. A lot of inspiration was drawn from 3b1b's manim library.

Reanimate aims at being a batteries-included way of gluing together different technologies: SVG as
a universal image format, LaTeX for typesetting, ffmpeg for video encoding, inkscape/imagemagick
for rasterization, potrace for vectorization, blender/povray for 3D graphics, and Haskell for
scripting.

# Getting started

## Prerequisites

Reanimate is built using the Haskell Tool Stack. For installation instructions, see: https://docs.haskellstack.org/en/stable/README/

Optionally, you can install one or more of these programs to enable additional features:
 * [ffmpeg](https://www.ffmpeg.org/), enables rendering animations to video files.
 * [latex](https://www.latex-project.org/), enables mathematical typesetting.
 * [inkscape](https://inkscape.org/)/[imagemagick](https://imagemagick.org/index.php), enables SVG->PNG convertions.
 * [potrace](http://potrace.sourceforge.net/), enables PNG->SVG tracing.
 * [povray](https://www.povray.org/), enables raytracing.
 * [blender](https://www.blender.org/), enables 3D graphics.

## Installing / Running an example

Reanimate ships with a web-based viewer and automatic code reloading. To get a small demo
up and running, clone the repository, run one of the examples (this will install the library),
and wait for a browser window to open:

```console
$ git clone https://github.com/Lemmih/reanimate.git
$ cd reanimate/
$ stack build
$ stack ./examples/doc_drawCircle.hs
```

This should render the `doc_drawCircle` example in a new browser window. If you then change the
animation source code, the browser window will automatically reload and show the updated animation.

# Documentation

 * API reference: http://hackage.haskell.org/package/reanimate/docs/Reanimate.html
 * Tutorial: https://reanimate.readthedocs.io/en/latest/glue_tut/
 * Gallery with source code: https://reanimate.readthedocs.io/en/latest/gallery/

# Examples

The example gifs are displayed at 25 fps.

![LaTeX wheel](gifs/latex_wheel.gif)
![Sunflower](gifs/sunflower.gif)
![Tangent](gifs/tangent.gif)
![Goo](gifs/goo.gif)
![Drawing LaTeX equations](gifs/latex_draw.gif)
![Bounding boxes](gifs/bbox.gif)
![Colorful LaTeX](gifs/latex_color.gif)
![Bezier curves](gifs/bezier.gif)
![Valentine's Day](gifs/valentine.gif)
![Basic LaTeX](gifs/latex_basic.gif)

# Authors

  * David Himmelstrup.
  * Jan Hrcek.

# License

This is free and unencumbered software released into the public domain.

Anyone is free to copy, modify, publish, use, compile, sell, or
distribute this software, either in source code form or as a compiled
binary, for any purpose, commercial or non-commercial, and by any
means.

# Acknowledgments

  * Huge thanks to 3b1b's [manim](https://github.com/3b1b/manim) which inspired this library.
  * Thanks to [svg-tree](https://github.com/Twinside/svg-tree) for their SVG library.
  * Thanks to [CthulhuDen/chiphunk](https://github.com/CthulhuDen/chiphunk) for making a 2D physics
    library easily available.

# YouTube

Completed animations are uploaded to the [Reanimated Science](https://www.youtube.com/channel/UCbZujyI7i6JbI-I0shPvDgg) channel.

Animation snippets are uploaded to the [Reanimated Science Playground](https://www.youtube.com/channel/UCL7MwXLtQbhJeb6Ts3_HooA) channel.
