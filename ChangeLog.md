# Revision history for reanimate

## 0.2.0.2 -- 2020-02-25

* Rewrite viewer from javascript to elm.
* Improve API documentation.
* Support for GeoJSON files (rendering borders, etc).
* Fix bug affecting GIF frame rates.
* Improve SVG serialization performance.
* Add support for transitions (ie. functions that merge animations).
* Properly handle ctrl-c when rendering.
* Improve accuracy of bounding-box approximation.
* Rewrite the API for building animations sequentially.
* Expand 'pathify' to work on a larger subset of SVG.
* Respect aspect ratio when 'height' or 'width' is specified but not both.

## 0.1.6.0 -- 2019-09-14

* Test suite.
* Improved Windows support.
* Automated builds on Linux, Mac, and Windows.
* Automated builds with lts-13, lts-12, and lts-11.
* Monadic language for composing animations (Reanimate.Scene).
* Support for Povray.
* Viewer: Incrementally generate videos with increasing fps.
* 'check' command to verify external dependencies.
* Built-in data for color theory (cone sensitivity etc).
* Built-in colormaps.
* Raster image support.
* Reorganized signals.
* Color interpolation (rgb, hsv, lab, xyz).
* Improved performance of multi-threaded renderer.

## 0.1.5.0 -- 2019-07-07

* Basic driver for live previewing and rendering.
* Basic tools for manipulating SVGs (pathification, simplification, lowering
  transformations, etc).
* Automated CI builds (azure).

## 0.1.0.0 -- 2019-03-08

* First version. Released on an unsuspecting world.
