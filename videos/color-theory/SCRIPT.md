On its own, data doesn't look like much.
Often it is merely a wall of unintelligible numbers.

However, if we take each number and assign it a shade of grey, suddenly
the data becomes understandable and the face of the monalisa stares out at us.

Using shades of grey is not the only possible color palette, though,
and many colorful alternatives exists. Some of these colormaps have been around
for a while, and Jet, one of the oldest, first appeared in the 1970s. However,
in recent years, a lot of attention has gone into addressing the shortcomings
of the early colormaps and create better standards for the future.
Particularly, Matlab replaced Jet by Parula in 2014, Viridis became the default
in Matplotlib by 2015, the research paper describing Cividis was published in
2018, and Google created Turbo as a spiritual successor to Jet in 2019.

So, why were so many new colormaps invented in this 5-year period? How are these
colormaps created? Are they merely the favorite colors of their creators? How
can a colormap be interesting enough to merit a research publication?

To answer these questions, first we have to explore a bit of color theory.

Visible light roughtly lies between 400 and 700nm, and the colors we see
in the natural world are usually made up of a mixture of all these wavelengths.
For example, a lotus leaf might emit a complicated light spectrum like this.
Fortunately, our eyes will simplify this jumble of data down to just three
values. This is because there are just three types of color sensing cones,
sensing short, medium and long wavelenghts, and thus named S, M and L.
All colors now have a position in the three dimensional space with S, M and L
as the axes. However, due to the overlap in cone sensitivities, this space
is quite awkward to work with and has lots of imaginary colors. For example,
this space has a position for the color that only stimulates the M-cone but no
light can stimulate that cone without also stimulating one or two other cones.
So, to make colors easier, the LMS space was stretched and
squeezed into a new shape, called the XYZ space.
Taking a 2D slice of this space gives a pyramid of colors with reds to the
right, greens at the top, and blues to the left.
This space still contains non-sensible colors but we can filter them out by
mapping each wavelength of visible light.
While this shape represents all the visible colors, computer can only show
the colors inside of this triangle and all the colors outside of this triangle
have been mere approximations.
Finally we arrive at a usable space of colors: sRGB.
Drawing a path through the sRGB space gives us a colormap but it doesn't
guarantee that colormap to be particularly good. The sRGB colorspace captures
how light turns into color but those colors don't have an intuitive layout.
Fortunately, we can just invent new color spaces on top of sRGB that have
different properties. For example, the Hue-Satuation-Value space makes it
easier to draw paths that smoothly transitions through hue, saturation and value.

For colormaps, the properties you want are perceptual smoothness, meaning no
sharp transitions in color, and a gradual increase in brightness. However,
smoothness and brightness are interpreted in the brain rather than the eyes, and
either the sRGB space nor the HSV space capture these properties particularly
well. For example, blue light looks darker than green light even if the same
number of photons hits your eyes. So, scientists asked a bunch of people which
colors they thought were different and which colors they thought were the
brightest, and the result is the LAB colorspace, a space that maps the visual
cortex of an average human.
