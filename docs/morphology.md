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

SVG images can be simplified to polygons which are made out of points. To morph
between two shapes, we somehow have to transform the points in a source
polygon into the points in a target polygon. Finding a good point correspondence
between two polygons is a tricky thing, though, and it can have a huge impact
on the quality of the morph as seen in this illustration:

<video width="640" height="360" muted autoplay loop>
  <source src="https://i.imgur.com/9WJ6mC6.mp4">
</video>

Point correspondence algorithms take two polygons (which may have a different
number of points) and align their points so there's an exact one-to-one
correspondence. This may require shuffling, adding, splitting, or merging points.

In *reanimate*, point correspondence algorithms have this type:
```haskell
type PointCorrespondence = Polygon → Polygon → (Polygon, Polygon)
```


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

Morphing between colors is something many libraries don't pay a lot of attention
but there is a fair amount of complexity here and it does deserve consideration.
In short, humans (usually) have three types light sensitive cells that respond
to different ranges of light wavelengths. So, even though the light we see may
contain hundreds of different wavelengths, the eye only notices how much each
of the three types of cells are stimulated and will therefore always reduce
colors to just three numbers.

So far, so good. With three coordinates necessary to specify a color, each color
has a position in a three dimensional space and we can morph between two colors
by going in a straight line. This is where things get tricky, though, because
going in a straight line in the XYZ space (the 3D colorspace defined by the
sensitivity of a typical human's eye) is physically correct but often a bit
counter intuitive. For example, morphing between **blue** and **yellow** will go
through **grey**. Also, the XYZ doesn't take into account how humans perceive
colors (ie. what happens in the brain rather than what happens in the eye) and
morphing between two colors with the same perceived brightness will not keep the
brightness a constant. These concerns, and many more, have lead to the
development of different 3D colorspaces, such as CIE LAB, which take human color perception into
account. *Reanimate* has built-in support for several spaces and this
illustration shows how they stack up against each other:

<video width="640" height="360" muted autoplay loop>
  <source src="https://i.imgur.com/AUqFi7L.mp4">
</video>

# Linear interpolation

TBD.

# Rotational interpolation

```haskell
rotationalTrajectory :: Origin -> Trajectory
```

<video width="640" height="360" muted autoplay loop>
  <source src="https://i.imgur.com/dFY0IZz.mp4">
</video>

<video width="640" height="360" muted autoplay loop>
  <source src="https://i.imgur.com/nAk8xJ1.mp4">
</video>

# Edge+angle interpolation

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

# Least-work correspondence

<video width="640" height="360" muted autoplay loop>
  <source src="https://i.imgur.com/C8bCnea.mp4">
</video>

$$
work_{stretch} = k_s\frac{\delta^2}{2L_0}
$$

$$
work_{bend} =
\left\{
\begin{array}{ll}
k_b(\Delta \theta + m_b \Delta \theta^\star)^{e_b}
  & \text{if $\theta(t)$ never goes to zero} \\
k_b(\Delta \theta + m_b \Delta \theta^\star)^{e_b} + p_b
  & \text{if $\theta(t)$ does go to zero}
\end{array}
\right.
$$

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
