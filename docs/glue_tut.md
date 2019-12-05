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

Scripting in Haskell also gives access to the extensive body of code libraries. There are Haskell libraries for syntax highlighting, font manipulation, and much, much more. In the spirit of being a batteries-included framework, Reanimate ships with a built-in 2D physics library, called Chipmunk2D. The example below demonstrates how SVG shapes can be used nearly effortlessly in a physics simulation.

<details>
  <summary>Toggle source code.</summary>
  <pre><code class="haskell">
  {!examples/tut_glue_physics.hs!}
  </code></pre>
</details>
<br/>
<video width="640" height="360" autoplay loop>
  <source src="../rendered/tut_glue_physics.mp4">
  <source src="https://github.com/Lemmih/reanimate/raw/master/docs/rendered/tut_glue_physics.mp4">
</video>


## Pillar II: LaTeX


LaTeX is a widely used system for typesetting equations and documents. It is most commonly used by writing TeX documents which are then converted to pdfs. However, since the output of LaTeX is natively vector graphics, it is trivial to get SVG documents instead of pdfs. Armed with this knowledge, Reanimate offers a simple yet powerful function: `latex :: Text -> SVG`

The `latex` function takes a snippet of TeX code, passes it through the LaTeX system, and converts the result to an SVG image. Furthermore, since the result is entirely determined by the TeX code, caching is used to hide the overhead of invoking LaTeX. 

The resulting SVGs can be manipulated just like any other. The below examples illustrates how different effects can be applied to different glyphs in the equation.

<details>
  <summary>Toggle source code.</summary>
  <pre><code class="haskell">
  {!examples/tut_glue_latex.hs!}
  </code></pre>
</details>
<br/>
<video width="640" height="360" autoplay loop>
  <source src="../rendered/tut_glue_latex.mp4">
  <source src="https://github.com/Lemmih/reanimate/raw/master/docs/rendered/tut_glue_latex.mp4">
</video>

## Pillar III: povray

TODO: Draw text and animate shapes, zoom out so it looks like a piece of paper (still animated).

<details>
  <summary>Toggle source code.</summary>
  <pre><code class="haskell">
  {!examples/tut_glue_povray.hs!}
  </code></pre>
</details>
<br/>
<video width="640" height="360" autoplay loop>
  <source src="../rendered/tut_glue_povray.mp4">
  <source src="https://github.com/Lemmih/reanimate/raw/master/docs/rendered/tut_glue_povray.mp4">
</video>

TODO: Talk about orthographic projection. Draw circle, fade in rotating sphere.

<details>
  <summary>Toggle source code.</summary>
  <pre><code class="haskell">
  {!examples/tut_glue_povray_ortho.hs!}
  </code></pre>
</details>
<br/>
<video width="640" height="360" autoplay loop>
  <source src="../rendered/tut_glue_povray_ortho.mp4">
  <source src="https://github.com/Lemmih/reanimate/raw/master/docs/rendered/tut_glue_povray_ortho.mp4">
</video>


## Pillar IV: Blender

TODO: Crumble SVG animation.

TODO: Morph SVG animation into sphere.

## Pillar V: potrace


