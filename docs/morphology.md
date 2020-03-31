# 2D morphology

Morphing 2D shapes is conceptually simple. We can image it as a function that
takes two SVG images and produces the intermediate steps that smoothly
transforms the source image to the target image. If the SVG images are simple
shapes then it looks like this:

<video width="640" height="360" muted autoplay loop>
  <source src="https://i.imgur.com/4Uwayst.mp4">
</video>

Often it is quite easy for a human to tell if a morph looks right or not. This,
unfortunately, does not mean the problem is easy to solve for a computer. As
it turns out, there is not a single, perfect algorithm for morphing between 2D
shapes. Instead, there are many different algorithms that each have their own
benefits and trade-offs. This article will specify the details of the
morphology-problem and show a handful of different solutions.

# Problem specification

Morphing between two shapes can be split into several smaller problems that can
be tackled separately:

* **Point correspondence:**
* **Trajectory:**
* **Object correspondence:**
* **Color interpolation:**
* **Attribute interpolation:**

    * position
    * color
    * stroke

<video width="640" height="360" muted autoplay loop>
  <source src="https://i.imgur.com/F7RZjJN.mp4">
</video>

# Linear interpolation

TBD.

# Stretch and Bend

TBD.

# Skeleton

TBD.

# As-rigid-as-possible

TBD.

# Guaranteed intersection-free

TBD.
