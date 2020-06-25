
# Reanimate is:
 * **An animation library** - a system for turning code into movies, similar to [3b1b's manim](https://github.com/3b1b/manim) of [YouTube fame](https://www.youtube.com/channel/UCYO_jab_esuFRV4b17AJtAw).
 * **User friendly** - extensively documented with example animations covering the [entire API](http://hackage.haskell.org/package/reanimate/docs/Reanimate.html).
 * **Cross-platform** - works on Linux, Mac and Windows.


# Getting started

## Viewing a basic animation

```console
$ git clone https://github.com/reanimate/reanimate.git
$ cd reanimate/
$ stack build
$ stack ./examples/doc_andThen.hs
```

Running the above code should open a browser window containing this animation:

<details>
  <summary>Toggle doc_andThen.hs source code.</summary>
  <pre><code class="haskell">
  {!examples/doc_andThen.hs!}
  </code></pre>
</details>
<br/>
<video style="width:100%" muted autoplay loop>
  <source src="https://i.imgur.com/bvLqalX.mp4">
</video>


## Checking dependencies

If something doesn't work, you can run a check for run-time dependencies like
this:

```console
$ stack ./examples/doc_andThen.hs check
reanimate checks:
  Has ffmpeg:                        4.1.3-0ubuntu1
  Has dvisvgm:                       /usr/bin/dvisvgm
  Has povray:                        /usr/bin/povray
  Has blender:                       2.82
  Has rsvg-convert:                  2.44.10
  Has inkscape:                      0.92.4
  Has convert:                       6.9.10-14
  Has LaTeX:                         /usr/bin/latex
  Has LaTeX package 'babel':         OK
  Has LaTeX package 'preview':       OK
  Has LaTeX package 'amsmath':       OK
  Has XeLaTeX:                       /usr/bin/xelatex
  Has XeLaTeX package 'ctex':        OK
```

None of these dependencies are vital but the functionality of **reanimate**
will be reduced if they are missing. For example, without 'ffmpeg', you won't
be able to generate stand-alone video files (but can still view animations
in a browser window). Without 'LaTeX' and 'dvisvgm', you won't be able use
LaTeX for typesetting.

## Rendering animations

**reanimate** has builtin support for rendering animations to video files and
gifs. To render an animation with default options, use the 'render' command:

```console
$ stack ./examples/doc_andThen.hs render
```

For computationally intense animations, use the `--compile` flag to compile
your animation script and run it using all available cores:

```console
$ stack ./examples/doc_andThen.hs render --compile
```

If the defaults are too aggressive for you tastes, change them directly or use
a different preset:

```console
$ stack ./examples/doc_andThen.hs render --preset high --fps 10
```

Rendering to gifs automatically does two passes and uses a color palette to
improve the quality and size of the gif. GIFs are, by default, rendered at
24 fps with a resolution of 320x180:

```console
$ stack ./examples/doc_andThen.hs render --format gif
```

# Learning resources

* [API documentation](http://hackage.haskell.org/package/reanimate/docs/Reanimate.html)
* [Core concepts](introduction)
* [Gallery](gallery)
* [Examples](https://github.com/reanimate/reanimate/tree/master/examples)
* [Design overview](glue_tut)
