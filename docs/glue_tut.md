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
 * Filter effects: Blur and blob.

TODO: Add advanced SVG features: blurs, blobs, etc.

<details>
  <summary>Toggle source code.</summary>
  <pre><code class="haskell">
  {!examples/tut_glue_svg.hs!}
  </code></pre>
</details>
<br/>
<video width="640" height="360" autoplay loop>
  <source src="https://github.com/Lemmih/reanimate/raw/master/docs/rendered/tut_glue_svg.mp4">
  <source src="../rendered/tut_glue_svg.mp4">
</video>

## Animation = Time 🡢 SVG

<details>
  <summary>Toggle source code.</summary>
  <pre><code class="haskell">
  {!examples/tut_glue_animate.hs!}
  </code></pre>
</details>
<br/>
<video width="640" height="360" autoplay loop>
  <source src="https://github.com/Lemmih/reanimate/raw/master/docs/rendered/tut_glue_animate.mp4">
  <source src="../rendered/tut_glue_animate.mp4">
</video>

## Pillar I: Haskell

TODO: Write about fourier drawings. About how advanced math is doable with Haskell.

## Pillar II: Libraries

TODO: Write about how lots of libraries are available for Haskell. Use chiphunk
      as an example. Show SVG primitives with 2D physics.

## Pillar III: LaTeX

TODO: Show that LaTeX is a provider of SVG graphics. It has the type
      'latex :: Text -> SVG'. Caching is automatic and it plays well with
      other SVG functions (partialSvg, center, etc).

## Pillar IV: potrace

## Pillar V: Povray

## Pillar VI: Blender
