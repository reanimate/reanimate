# Core concepts

This document will introduce the foundamental concepts used in **reanimate**.
It is assumed that you are already familiar with Haskell. After reading, you will be able to understand all the concepts used in the [API reference documentation](http://hackage.haskell.org/package/reanimate/docs/Reanimate.html). This is not a tutorial, though, and you may have to look at the examples before you can turn these concepts into beautiful animations.

For a bird's eye view on the design philosophy and capabilities of **reanimate**, see [Gluing together animations](/glue_tut/). For help installing **reanimate**, see [Getting started](/#getting-started).

## Animations as Executables

**Reanimate** animations are self-contained executables. These executables are
capable of rendering the compiled animation in many different formats (mp4, gif, web)
and with different resolutions and framerates. In other words, animations are agnostic
with respect to the output format, the resolution, and the framerate.

By default, when you run the executable, the animation will open and play in a new browser window. Running the executable with `render` will create an .mp4 file with the same basename as the source file. For more details, have a look at the [driver documentation](http://hackage.haskell.org/package/reanimate/docs/Reanimate.html#v:reanimate).

Let's get our feet wet and have a look at an animation written with **reanimate**.
By the way, all animations in this document are available from the [`examples/`](https://github.com/Lemmih/reanimate/tree/master/examples) folder in the
project repository. Playing with the examples is a good way to learn.

We'll start with a minimal animation that prints "Hello world" on top of a cyan background, and then go over what the code means:

<details>
  <summary>Toggle source code.</summary>
  <pre><code class="haskell">{!examples/intro_hello.hs!}</code></pre>
</details>
<video width="640" height="360" muted autoplay loop>
  <source src="https://i.imgur.com/dJIl8dy.mp4">
</video><pre><code class="haskell">{!examples/intro_hello.hs!}</code></pre>

Let's go over the five library functions used by the Hello World animation:

* [`mkText       :: Text -> SVG`](http://hackage.haskell.org/package/reanimate/docs/Reanimate-Svg-Constructors.html#v:mkText)<br/>
   Each frame in the animation is an SVG image so we need a helper function to
   construct an SVG text node.
* [`staticFrame  :: Duration -> SVG -> Animation`](http://hackage.haskell.org/package/reanimate/docs/Reanimate.html#v:staticFrame)<br/>
   Our animation has no moving parts so we use the `staticFrame 1` call to
   say that we want the same SVG image for `1` full second.
* [`mkBackground :: String -> SVG`](http://hackage.haskell.org/package/reanimate/docs/Reanimate-Svg-Constructors.html#v:mkBackground)<br/>
   This helper function constructs a rectangle the size of the animation with a specific color.
* [`addStatic    :: SVG -> Animation -> Animation`](http://hackage.haskell.org/package/reanimate/docs/Reanimate.html#v:addStatic)<br/>
   The background is a static component we're adding to our animation.
* [`reanimate    :: Animation -> IO ()`](http://hackage.haskell.org/package/reanimate/docs/Reanimate.html#v:reanimate)<br/>
   Finally, we invoke the main driver to turn the `Animation` into an executable.

## The Canvas

The default SVG coordinate system places <0,0> in the top-left corner, sets width
and height to match the output pixel resolution, and has the Y coordinate grow downwards.
To make animations agnostic to resolution, **reanimate** uses its own coordinate system:

* 16 units wide,
* 9 units high,
* 16/9 aspect ratio,
* Y axis grows upwards,
* X axis grows left to right,
* <0,0> placed at center of screen.

<details>
  <summary>Toggle source code.</summary>
  <pre><code class="haskell">{!examples/intro_canvas.hs!}</code></pre>
</details>
<video width="640" height="360" muted autoplay loop>
  <source src="https://i.imgur.com/E2hrppM.mp4">
</video><br/>

Many of the SVG constructors such as [mkCircle](http://hackage.haskell.org/package/reanimate/docs/Reanimate-Svg-Constructors.html#v:mkCircle) and [mkRect](http://hackage.haskell.org/package/reanimate/docs/Reanimate-Svg-Constructors.html#v:mkRect) do not take positioning arguments and are instead always centered at <0,0>. They can be moved with [translate](http://hackage.haskell.org/package/reanimate/docs/Reanimate-Svg-Constructors.html#v:translate), though.

Also, if the default coordinate system or the 16/9 aspect ratio is unsuitable for your needs, they can easily be changed. See [Custom viewports](#custom-viewports) for details.

## Animations

Animations describe how SVG frames change over a finite amount of time. There's both a [declarative API](http://hackage.haskell.org/package/reanimate/docs/Reanimate.html#g:2) and an [imperative API](http://hackage.haskell.org/package/reanimate/docs/Reanimate.html#g:4) for constructing and composing animations, and they are often used in conjunction with each other.

## Signals

Signals are also called easing functions. They modify the rate of change for animations and mutable variables. Graphical examples are available in the [API documentation](http://hackage.haskell.org/package/reanimate/docs/Reanimate.html#g:3). More examples are [covered here](https://easings.net/).

## Scenes

The [scene API](http://hackage.haskell.org/package/reanimate/docs/Reanimate.html#g:4) offers an imperative method for composing animations. It has sprites and mutable variables, yet is still free of side-effects and can be used freely with declarative combinators and effects.

## Text and LaTeX

SVG text nodes can be undesirable for two reasons: (a) How they are rendered depends on available system fonts, (b) manipulating them as glyphs is difficult from Haskell (eg. finding the height of a text node or converting a text node to curves). Luckily, LaTeX can output SVG files. LaTeX is great at typesetting, especially for mathematics, but there are a few caveats:

 * Font metrics such as baseline, point size, ascender, descender, etc, are unavailable and aligning text is therefore more difficult.
 * SVG features such as wrapping text around a curve are unavailable.
 * Invoking LaTeX can be slow (taking a second or more).
 * LaTeX may be unavailable.

 While most of these drawbacks are still unsolved, **reanimate** does have builtin caching that hides the cost of calling LaTeX.

## Custom viewboxes

**Reanimate** defaults to a 16 by 9 aspect ratio but is capable of generating videos and GIFs of any resolution. You can create custom aspect ratios by scaling your animation to fit
[screenWidth](http://hackage.haskell.org/package/reanimate/docs/Reanimate.html#v:screenWidth) by [screenHeight](http://hackage.haskell.org/package/reanimate/docs/Reanimate.html#v:screenHeight), or by using the [withViewBox](http://hackage.haskell.org/package/reanimate/docs/Reanimate-Svg-Constructors.html#v:withViewBox) helper function.

<details>
  <summary>Toggle source code.</summary>
  <pre><code class="haskell">{!examples/intro_canvas_square.hs!}</code></pre>
</details>
<video width="360" height="360" muted autoplay loop>
  <source src="https://i.imgur.com/XaKIn8W.mp4">
</video>
