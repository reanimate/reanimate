# Gluing together animations

Reanimate is a library for programmatically generating animations with a twist
towards mathematics / vector drawings. A lot of inspiration was drawn from
3b1b's manim library.

Reanimate aims at being a batteries-included way of gluing together different technologies: SVG as a universal image format, LaTeX for typesetting, ffmpeg for video encoding, inkscape/imagemagick for rasterization, potrace for vectorization, blender/povray for 3D graphics, and Haskell for scripting.

## Scalable Vector Graphics

Movies consists of a sequence of frames and, in reanimate, these frames are SVG images. SVGs can easily reference raster images, includes a set of drawing primitives, and offers image advanced manipulation through filter effects.
Since SVGs are plain-text documents, tools can be written to analyse and modify images. For example, reanimate includes code for applying 2D physics to shapes in SVG images.

SVG features, as demonstrated in the below animation:

 * Drawing primitives: Circles, rectangles, lines, external images, paths, text.
 * Drawing attributes: Rotation, position, color, line-width.
 * Filter effects: Blur and blob (merging shapes).

<details>
  <summary>Toggle source code.</summary>
  <pre><code class="haskell">
  {!examples/tut_glue_svg.hs!}
  </code></pre>
</details>
<br/>
<video width="640" height="360" autoplay loop>
  <source src="../rendered/tut_glue_svg.mp4">
  <source src="https://github.com/Lemmih/reanimate/raw/master/docs/rendered/tut_glue_svg.mp4">
</video>

## Animation = Time ➞ SVG

Animations can be defined as SVG images over time (plus a bit of bookkeeping such as their duration). With this approach, the time variable can determine SVG properties such as radius, path lengths, rotation, and color. Reanimate ships with a bunch of combinators for composing and arranging animations.

<details>
  <summary>Toggle source code.</summary>
  <pre><code class="haskell">
  {!examples/tut_glue_animate.hs!}
  </code></pre>
</details>
<br/>
<video width="640" height="360" autoplay loop>
  <source src="../rendered/tut_glue_animate.mp4">
  <source src="https://github.com/Lemmih/reanimate/raw/master/docs/rendered/tut_glue_animate.mp4">
</video>

Reanimate is not an opinionated framework, though, and also offers a more traditional keyframing tools. The example below uses an imperative API to schedule the various sub animations, transitions, and effects.

<details>
  <summary>Toggle source code.</summary>
  <pre><code class="haskell">
  {!examples/tut_glue_keyframe.hs!}
  </code></pre>
</details>
<br/>
<video width="640" height="360" autoplay loop>
  <source src="../rendered/tut_glue_keyframe.mp4">
  <source src="https://github.com/Lemmih/reanimate/raw/master/docs/rendered/tut_glue_keyframe.mp4">
</video>


## Pillar I: Haskell

A large part of Reanimate's expressive power comes from using Haskell as the scripting language. Haskell tends to favor expressiveness and correctness over raw performance and that is exactly what is needed from a glue language. All the heavy-lifting of rendering frames and encoding videos is handled by external tools and Reanimate merely needs to concern itself with finding intuitive ways of describing animations.

The following examples shows how something as seemingly complicated as fourier series can be expressed and animated in Haskell. Most noteworthy is the layered structure of the code:

 1. The first layer handles the mathematics of fourier series without worrying about graphics or how properties should be animated,
 2. the second layer describes how to draw a single frame given the length of the fourier series and the degree of rotation,
 3. the third layer deals with time: order of animations, durations, number of rotations, transition timing functions, pauses, etc.

<details>
  <summary>Toggle source code.</summary>
  <pre><code class="haskell">
  {!examples/tut_glue_fourier.hs!}
  </code></pre>
</details>
<br/>
<video width="640" height="360" autoplay loop>
  <source src="../rendered/tut_glue_fourier.mp4">
  <source src="https://github.com/Lemmih/reanimate/raw/master/docs/rendered/tut_glue_fourier.mp4">
</video>


TODO: Write about how lots of libraries are available for Haskell. Use chiphunk
      as an example. Show SVG primitives with 2D physics.

## Pillar II: LaTeX

TODO: Show that LaTeX is a provider of SVG graphics. It has the type
      'latex :: Text -> SVG'. Caching is automatic and it plays well with
      other SVG functions (partialSvg, center, etc).

## Pillar III: potrace

## Pillar IV: Povray

## Pillar V: Blender
