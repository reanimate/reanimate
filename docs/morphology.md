# 2D morphology

Morphing 2D shapes is conceptually simple. We can imagine it as a function that
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

# Breaking it down

Conceptually, a morphing function should take two SVG images and give a new
SVG image over time. In Haskell, it would look like this:

```haskell
morph :: SVG → SVG → (Time → SVG)
```

However, morphing has several sub-problems, each of which that can be solved
indepently in a variety of ways. These solutions have different benefits and
drawbacks, and wildly different performance. Since no single morphing algorithm
is obviously superior in all cases, let's identify the othogonal sub-problems
and use their solutions as parameters to our morphing function.

## 1. Point correspondence

SVG images can be simplified to polygons

```haskell
type PointCorrespondence = Polygon → Polygon → (Polygon, Polygon)
```

<video width="640" height="360" muted autoplay loop>
  <source src="https://i.imgur.com/9WJ6mC6.mp4">
</video>

## 2. Point trajectory

```haskell
type Trajectory = (Polygon, Polygon) → (Double → Polygon)
```

<video width="640" height="360" muted autoplay loop>
  <source src="https://i.imgur.com/jXOR7Ij.mp4">
</video>

## 3. Object correspondence

```haskell
type ObjectCorrespondence = [Polygon] → [Polygon] → [(Polygon, Polygon)]
```

<video width="640" height="360" muted autoplay loop>
  <source src="https://i.imgur.com/GoiZxgo.mp4">
</video>

## 4. Color interpolation

<video width="640" height="360" muted autoplay loop>
  <source src="https://i.imgur.com/F7RZjJN.mp4">
</video>

## 5. Attribute interpolation

* stroke
* opacity


# Linear interpolation

TBD.

# Rotational interpolation

TBD.

# Stretch and Bend

TBD.

# Interpolating lengths and angles

```haskell
lineBend :: Trajectory
```

<video width="640" height="360" muted autoplay loop>
  <source src="https://i.imgur.com/cmG1Wwr.mp4">
</video>

<video width="640" height="360" muted autoplay loop>
  <source src="https://i.imgur.com/hoSncAC.mp4">
</video>


TBD.

# Skeleton

TBD.

# As-rigid-as-possible

TBD.

# Guaranteed intersection-free

TBD.

# Related work

* Flubber
* Polymorph
* raphael
* Kute
* svg_morph
* D3
