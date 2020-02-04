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


The wavelength of visible light is roughly between 400nm and 700nm, and the
colors we see in the natural world tends to span this entire range.
As such, a lotus leaf might emit a light spectrum like this. However, eyes
don't detect indivial wavelengths and instead are sensitive to ranges
of light.
There are three types of color-sensing cones, S, M, and L, sensitive
to short, medium, and long wavelengths respectively.

Now imagine a space with S, M, and L as the axes

The intensity of light sensed by these three cones give each color a unique
three dimensional position. However, due to the rather large overlap between
the M and L cones, the resulting three dimensional LMS space is awkward to use.
This, plus the fact that blue is perceived to be much less bright than
other colors, lead to the development of the XYZ colorspace. The XYZ space
is equivalent to the LMS space but has been designed with a focus on the colors
a human can actually perceive.

The colors in the XYZ space form a pyramid in three dimensions but
we can take a slice through this pyramid show a triangular section of colors
with roughly equal intensity.

The X corner has all the red colors, the Y corner has green colors, and the
Z corner has blue colors.

This is a false picture of colors, though, and some areas of the triangle cannot be
perceived. This is because cone sensitivities overlap and even pure laser light
of a single frequency would activate more than one.

Plotting the wavelengths of visible light removes the false colors and gives us
a map of every perceivable color.
