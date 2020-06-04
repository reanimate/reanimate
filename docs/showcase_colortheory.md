# Color theory

<iframe width="560" height="315" src="https://www.youtube.com/embed/txSAC6mJQDk" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>

Reanimate has built-in support for several different colormaps like viridis and parula. This got me quite interested
in the color theory that underpins them and I decided to make a short video presenting what I learned. This is not
about color theory, thought, but rather the technical details involved with writing and rendering a fairly long
animation in Haskell.


# Caching

Reanimate uses the conceptually simple model that animations are SVG images over time. This
model makes it easy to create animations but it can also be quite inefficient. In the worst case,
graphical elements are recomputed in every frame instead of being computed once and then reused.
The color-theory video contains a programmatically generated map of the XYZ color space. This map
is quite expensive to generate at high resolutions but it almost never changes and is therefore a
prime candidate for caching.

Videos don't have to be particular long before render times become a serious concern. The
color-theory video is 6m13s long, consisting of 22403 frames at 60 frames per second, rendered
at a resolution of 2560x1440. Even at this small scale, rendering without any caching is
infeasible, clocking in at about **10 hours** with my (admittedly underpowered) 4 core / 8
threads i7-8705 CPU.

When a large SVG node is repeated in many frames, it can be much faster to convert it to
an image and then replacing it with the image file in subsequent frames. Reanimate has
builtin support for this kind of pre-rendering:
```haskell
prerenderSvg :: Hashable a => a -> SVG -> SVG
```
The first argument to `prerenderSvg` is a key that must uniquely identify the SVG. This
could be the hash of the SVG but is usually another value that is cheaper to produce.
The returned SVG node contains the PNG image of the rendered input. Caching is handled
transparently and repeated calls will only render the SVG once.

Pre-rendering just three large SVG nodes brings the render time down from ~10 hours to
**48 minutes** (12m 26s for generating SVGs + 36m 14s for encoding the video). 
A huge improvement and the code is still as declarative and maintainable
as before.

# External renderers

FFmpeg is the video encoder used by reanimate and it is responsible for turning SVG frames
into videos. Mostly it does a wonderful job but it is quite strict about security and does
not allow links from out SVG frames to external image files. Reanimate bypasses this
restriction by detecting when links are allowed and inlining images if they are not.

Inlining images (encoded as base64) is not cheap, though, and you have enough images
then it might be faster to use an external renderer before encoding the frames with
FFmpeg. Reanimate can use inkscape, rsvg and imagemagick to convert SVG images to
PNGs and those three renderers all support external links.

Using an external renderer should speed up both the generation of SVG frames and
the video encoding. This comes at the cost of adding an extra step for converting
SVG images to PNGs:

| renderer      | SVG ➞ PNG  |
| ------------- | ----------:|
| imagemagick   | 49m 11s    |
| inkscape      | 1h 39m 11s |
| rsvg          | 27m 35s    |

Imagemagick and inkscape are too slow but rsvg looks promising. Putting it all together
lowers the total render time from 48 minutes to **43 minutes.**

| approach      | SVG       | SVG ➞ PNG  | MP4     | Total       |
| ------------- | ---------:| ----------:| -------:| -----------:|
| no cache      | ~10 hours | -          | 36m 14s | ~10.5 hours |
| cache         | 12m 26s   | -          | 36m 14s | 48m 40s     |
| cache + rsvg  | 7m 56s    | 27m 35s    | 7m 45s  | 43m 16s     |

All-in-all, I'm pretty happy with a 6 minute video (60fps, UHD) requiring only 43 minutes to render. But there are still lots of room for improvement in the future:

  * There are quite a few duplicate frames in the color-theory video where nothing
    changes. These duplicates should be automatically identified and only rendered
    once.
  * Invoking an external renderer for each of the 22403 frames carries a lot of
    overhead. This overhead can be reduced by joining SVG frames together side-by-side
    and slicing up the resulting image.
  * SVG rendering is String-based and quite slow. Using the blaze-svg library will
    make it faster to serialize SVGs.

# Timings and script

I wrote nearly all of the color-theory animation before Reanimate had
[voice control](voice.md), and working out appropriate timings by hand was
way more difficult than I expected. It even changed my development
process. I used to write small fragments of animation and then combined them
into larger segments. Now I focus more on
[sprites](http://hackage.haskell.org/package/reanimate/docs/Reanimate-Scene.html#g:3),
their parameters, and their lifetime effects, and leave all timing/duration to
be settled at a later time.

Having computer-generated transcript timings and a speedy renderer is without a
doubt convenient. But I've found that writing an engaging and coherent script is
the most difficult task, by far. Not sure what Reanimate can do to help besides
taking care of technical details, thus lowering my cognitive load.
