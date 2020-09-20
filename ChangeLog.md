# Revision history for reanimate

Notable changes to the project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/) and the
project adheres to the [Haskell Package Versioning
Policy (PVP)](https://pvp.haskell.org)

## 1.0.0.0 -- 2020-09-20

### Changed

 * Objects: Use Linear.V2 instead of tuples.
 * Reanimate.Scene.sceneAnimation -> Reanimate.Scene.scene

### Other/Non-visible

 * Improved object documentation.
 * CI stability improvements.
 * Add interactive tutorial.
 * Minor playground improvements.

## 0.5.0.0 -- 2020-09-09

### Added

* Hashable instance for SVG.

### Changed

* Improve efficiency of time variables (thanks to Shaurya Gupta).
* Major refactoring of SVG interface.
* Haddock improvements.
* Improve consistency of object bounding-box calculations.
* Rewrite object transitions in terms of Animations.

### Removed

* Reanimate.Scene.tweenVarUnclamped
* Reanimate.Math.SSSP.pdual

### Contributors

 * Shaurya Gupta (@sureyeaah)
 * Jan Hrček (@jhrcek)

## 0.4.3.0 -- 2020-08-29

* Improve documentation.

## 0.4.2.0 -- 2020-08-27

* Web-viewer: Use GHCi to reload code, reducing latency by ~100x.
* Improve caching for blender and povray.
* Hide many internal modules.
* Move hmatrix dependency to a separate package.
* Use hgeometry instead of broken C++ code.
* Introduce the basics for a new graphical object system.
* Introduce online playground.
* Introduce interactive embedded player.
* Hide internal modules and bring documentation coverage up to 100%.

## 0.4.1.0 -- 2020-07-30

* Slightly improved error messages.

## 0.4.0.0 -- 2020-07-28

* Show progress when running ffmpeg.
* Small improvements to the scene API.
* Bug fixes to the voice API.
* Support ImageMagick version 7.
* Improved Windows build support.
* Improved error reporting in web-viewer.
* Improved error messages when ffmpeg doesn't support SVGs.
* Improved filter-effect support.
* Generalized LaTeX support.
* New experimental object system.
* Discord bot capable of rendering animations in chat.
* Lots of assorted bug fixes and minor improvements.

## 0.3.3.0 -- 2020-05-22

* Voice control via the Gentle forced-aligner.
* Rename Reanimate.Interpolate to Reanimate.ColorComponents
* Rename Reanimate.Signals to Reanimate.Ease
* Fix positioning of latex output such that the baseline starts at (0,0)

## 0.3.2.0 -- 2020-05-16

* Fix bug that forced GIF width to 320 pixels.
* Add helper for creating custom viewboxes, withViewBox.
* Add newSprite_, newSpriteSVG_.
* Drop support for lts-11 and lts-12. Lts-15, lts-14 and lts-13 are supported.
* Add common interface for polygon morphing.

## 0.3.1.0 -- 2020-05-12

* Expose 'mkImage'

## 0.3.0.0 -- 2020-05-11

* Improve README.md with better examples at a higher framerate.
* Improve canvas documentation, courtesy of William Yao.
* Fix 'renameFile' bug when moving files between different file-systems.
* Improve GeoJSON performance.
* Improve SVG rendering performance.
* CLI: Show time spent and time remaining when rendering.
* Better support for external images.
* Support external raster engines (inkscape, image magick, rsvg).
* Fix framerate bug affecting GIFs.
* Improve boundingbox performance.
* Add generalized cubic bezier signal.

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
