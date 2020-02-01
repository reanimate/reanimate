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


Visible light roughly ranges from a wavelength of 400nm to 700nm. If each
combination of wavelengths gave rise to a unique color then creating a color
space would be nigh impossible. Fortunately, most human eyes have just three
types of light-sensitive cells that respond to ranges of wavelengths, and the
space of colors is therefore reduced to three dimensions.
The axes are called S, M and L because the corrosponding cones are sensitive to
short, medium, and lone wavelengths respectively.
