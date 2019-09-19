[![Hackage](https://img.shields.io/hackage/v/reanimate.svg)](http://hackage.haskell.org/package/reanimate)
[![Build Status](https://dev.azure.com/lemmih0612/reanimate/_apis/build/status/Lemmih.reanimate?branchName=master)](https://dev.azure.com/lemmih0612/reanimate/_build/latest?definitionId=1&branchName=master)
[![Documentation Status](https://readthedocs.org/projects/reanimate/badge/?version=latest)](https://reanimate.readthedocs.io/en/latest/?badge=latest)

# reanimate

Reanimate is a reactive framework for creating non-interactive animations from SVG images.
This package consists of a set of combinators, a renderer (using ffmpeg), and a web-based
previewer. Inline latex code is supported when 'latex' and 'dvisvgm' are installed.

Nothing about the API is stable at this point.

# YouTube

Completed animations are uploaded to the [Reanimated Science](https://www.youtube.com/channel/UCbZujyI7i6JbI-I0shPvDgg) channel.

Animation snippets are uploaded to the [Reanimated Science Playground](https://www.youtube.com/channel/UCL7MwXLtQbhJeb6Ts3_HooA) channel.

# Getting started

Reanimate ships with a web-based viewer and automatic code reloading. To get a small demo
up and running, clone the repository, run one of the examples (this will install the library),
and wait for a browser window to open:

```console
$ git clone https://github.com/Lemmih/reanimate.git
$ cd reanimate/
$ stack build
$ stack ./examples/latex_color.hs
```

This should render the `latex_color` example in a new browser window. If you then change the
animation source code, the browser window will automatically reload and show the updated animation.

# TODO

* Improve bounding box approximations
* Alignment and positioning combinators
* Figure out why performance doesn't scale linearly with more cores
   - diagrams are slow because they are rendered as a bytestring and then parsed as SVG. Find a way to shortcut.
* Test-suite: Compile and render all the example animations on the build servers. The SVG output should be stable.
* API for generating slides.
* Warn if 'group' nodes are found in 'clippath's.
* Optionally use gifski for high quality gifs: https://gif.ski/
* Driver:
   - Flags for selecting fps in web viewer
   - Detect if there's already an open browser window. If there is, don't open another one.
   - Watch all code files in the same directory (and sub-directories) as the source code.
* Viewer:
   - Keys for pausing, moving frame-by-frame, resetting.
* Documentation:
   - Tutorials
     - Coordinate system
     - "Draw" effect
     - Pixel perfect povray (orthographic projection)
   - Gallery

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
