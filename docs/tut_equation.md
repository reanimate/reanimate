# Introduction

This tutorial will guide you through the steps to set up a simple animation and teach you the basics of timing, positioning, and $\LaTeX$. It is assumed that you are familiar with [Haskell](https://www.haskell.org/) but you don't have to know anything about reanimate or animation in general.

All of the code snippets are interactive and require JavaScript.

# Tutorial Goal

By the end of this tutorial, we will have a short animation that shows an equation and then draws an explanation of each term:

<video width="640" height="320" muted autoplay loop style="display:block;margin:auto;">
  <source src="https://i.imgur.com/5a78Ot5.mp4">
</video>

<br/>














# Step 1: Blank canvas

To begin with, let's draw a light-blue background and make it easier to see our animation canvas. We can do this in the imperative scene context by creating a sprite containing the background color. All [SVG color names](https://developer.mozilla.org/en-US/docs/Web/CSS/color_value) are supported so give `mistyrose`, `aquamarine` or `burlywood` a try. Don't try hex codes, though, as they are beyond the scope of this tutorial.

<pre class="interactive">
animation :: Animation
animation = scene $ do
  newSpriteSVG_ $
    mkBackground "lightblue"
</pre>

API references: [scene](https://hackage.haskell.org/package/reanimate-0.5.0.1/docs/Reanimate-Scene.html#v:scene), [newSpriteSVG_](https://hackage.haskell.org/package/reanimate-0.5.0.1/docs/Reanimate-Scene.html#v:newSpriteSVG_), [mkBackground](https://hackage.haskell.org/package/reanimate-0.5.0.1/docs/Reanimate-Svg-Constructors.html#v:mkBackground), [Animation](https://hackage.haskell.org/package/reanimate-0.5.0.1/docs/Reanimate.html#t:Animation).













# Step 2: Typesetting an equation

Let's start by refactoring the previous animation and create a separate function that defines our environment. This will make it easier to modify the environment in the future:

```haskell
env :: Animation -> Animation
env = addStatic bg

bg :: SVG
bg = mkBackground "lightblue"
```

SVG has no support for typesetting equations but reanimate offers a convenient interface to LaTeX via `latexAlign :: Text -> SVG`. To place the equation at the center of the screen with the right size, we'll use `scale :: Double -> SVG -> SVG` and `center :: SVG -> SVG`. Try using `centerX :: SVG -> SVG`, changing the scaling factor, or using a different equation like `\\frac{\\infty}{\\pi}`.

<pre class="interactive">
animation :: Animation
animation = env $ scene $ do
  newSpriteSVG_ equation
  wait 1

equation :: SVG
equation = scale 3 $ center $
  latexAlign "E = mc^2"

env :: Animation -> Animation
env = addStatic bg

bg :: SVG
bg = mkBackground "lightblue"
</pre>
API references: [addStatic](https://hackage.haskell.org/package/reanimate-0.5.0.1/docs/Reanimate.html#v:addStatic), [center](https://hackage.haskell.org/package/reanimate-0.5.0.1/docs/Reanimate-Svg-Constructors.html#v:center), [scale](https://hackage.haskell.org/package/reanimate-0.5.0.1/docs/Reanimate-Svg-Constructors.html#v:scale), [latexAlign](https://hackage.haskell.org/package/reanimate-0.5.0.1/docs/Reanimate.html#v:latexAlign)
























# Step 3: Accessing glyphs

So far, so good. We have an environment with a blue background color and we have an equation in SVG format. It's almost time to start animating but first we need to split the equation SVG into smaller pieces. We can use the `splitGlyphs :: [Int] -> SVG -> (SVG, SVG)` to access a set of child nodes of an SVG by index.

<pre class="interactive">
animation :: Animation
animation = env $ scene $ do
  play $ staticFrame 1 equation
  play $ staticFrame 1 symb_e
  play $ staticFrame 1 symb_eq
  play $ staticFrame 1 symb_m
  play $ staticFrame 1 symb_c2

symb_e :: SVG
symb_e = snd $
  splitGlyphs [0] equation

symb_eq :: SVG
symb_eq = snd $
  splitGlyphs [1] equation

symb_m :: SVG
symb_m = snd $
  splitGlyphs [2] equation

symb_c2 :: SVG
symb_c2 = snd $
  splitGlyphs [3,4] equation

equation :: SVG
equation = scale 3 $ center $
  latexAlign "E = mc^2"

env :: Animation -> Animation
env = addStatic bg

bg :: SVG
bg = mkBackground "lightblue"
</pre>
API references: [splitGlyphs](https://hackage.haskell.org/package/reanimate-0.5.0.1/docs/Reanimate-Svg.html#v:splitGlyphs), [staticFrame](https://hackage.haskell.org/package/reanimate-0.5.0.1/docs/Reanimate.html#v:staticFrame), [play](https://hackage.haskell.org/package/reanimate-0.5.0.1/docs/Reanimate.html#v:play).























# Step 4: Moving objects

Objects in reanimate encapsulate SVG nodes and makes it easier to animate properties such as position and scale. In the example below, we create an object from the `E` SVG node and 'tween' position properties to make it move around the canvas. [Tweening](https://en.wikipedia.org/wiki/Inbetweening) (or inbetweening) is an animation concept that simply meanings moving smoothly from one value to another.

Reanimate exports many properties such as `oLeftX`, `oTopY` and `oCenterXY`. These properties manipulate the object's position with respect to its bounding box and margins. The position of an object is ultimately captured in the `oTranslate` property and resetting this property will undo any movements.

<pre class="interactive">
animation :: Animation
animation = env $ scene $ do
  newSpriteSVG_ equation
  obj <- oNew symb_e
  oShow obj

  oTweenS obj 1 moveTopLeft
  oTweenS obj 1 moveTopRight
  oTweenS obj 1 moveBotRight
  oTweenS obj 1 moveBotLeft
  oTweenS obj 1 moveToOrigin

moveTopLeft t = do
  oLeftX %= \origin ->
    fromToS origin screenLeft t
  oTopY %= \origin ->
    fromToS origin screenTop t

moveTopRight t = do
  oRightX %= \origin ->
    fromToS origin screenRight t
  oTopY %= \origin ->
    fromToS origin screenTop t

moveBotRight t = do
  oRightX %= \origin ->
    fromToS origin screenRight t
  oBottomY %= \origin ->
    fromToS origin screenBottom t

moveBotLeft t = do
  oLeftX %= \origin ->
    fromToS origin screenLeft t
  oBottomY %= \origin ->
    fromToS origin screenBottom t

moveToOrigin t = do
  (oTranslate._1) %= \origin ->
    fromToS origin 0 t
  (oTranslate._2) %= \origin ->
    fromToS origin 0 t

symb_e :: SVG
symb_e = snd $
  splitGlyphs [0] equation

equation :: SVG
equation = scale 3 $ center $
  latexAlign "E = mc^2"

env :: Animation -> Animation
env = addStatic bg

bg :: SVG
bg = mkBackground "lightblue"
</pre>
API references: [oTweenS](https://hackage.haskell.org/package/reanimate-0.5.0.1/docs/Reanimate-Scene.html#v:oTweenS), [oNew](https://hackage.haskell.org/package/reanimate-0.5.0.1/docs/Reanimate-Scene.html#v:oNew), [oShow](https://hackage.haskell.org/package/reanimate-0.5.0.1/docs/Reanimate-Scene.html#v:oShow), [oLeftX](https://hackage.haskell.org/package/reanimate-0.5.0.1/docs/Reanimate-Scene.html#v:oLeftX), [oTopY](https://hackage.haskell.org/package/reanimate-0.5.0.1/docs/Reanimate-Scene.html#v:oTopY), [oRightX](https://hackage.haskell.org/package/reanimate-0.5.0.1/docs/Reanimate-Scene.html#v:oRightX), [oBottomY](https://hackage.haskell.org/package/reanimate-0.5.0.1/docs/Reanimate-Scene.html#v:oBottomY), [oTranslate](https://hackage.haskell.org/package/reanimate-0.5.0.1/docs/Reanimate-Scene.html#v:oTranslate), [screenBottom](https://hackage.haskell.org/package/reanimate-0.5.0.1/docs/Reanimate.html#v:screenBottom), [screenLeft](https://hackage.haskell.org/package/reanimate-0.5.0.1/docs/Reanimate.html#v:screenLeft), [screenRight](https://hackage.haskell.org/package/reanimate-0.5.0.1/docs/Reanimate.html#v:screenRight), [screenTop](https://hackage.haskell.org/package/reanimate-0.5.0.1/docs/Reanimate.html#v:screenTop).



Now that we know how to move around an object, let's move each symbol in the equation to the left of the screen and scale them down slightly to make it a better fit. The animation looks a lot better if moves overlap slightly and this can be achieved by running them in parallel with `fork` and inserting a `wait 0.3` in between them.

<pre class="interactive">
animation :: Animation
animation = env $ scene $ do
  symbols <- mapM oNew
    [symb_e, symb_eq, symb_m, symb_c2]
  mapM_ oShow symbols

  forM_ (zip symbols yPositions) $
    \(obj, yPos) -> do
    fork $ oTweenS obj 1 $ \t -> do
      oScale %= \origin ->
        fromToS origin scaleFactor t
      oLeftX %= \origin ->
        fromToS origin screenLeft t
      oCenterY %= \origin ->
        fromToS origin yPos t
    wait 0.3

symb_e :: SVG
symb_e = snd $
  splitGlyphs [0] equation

symb_eq :: SVG
symb_eq = snd $
  splitGlyphs [1] equation

symb_m :: SVG
symb_m = snd $
  splitGlyphs [2] equation

symb_c2 :: SVG
symb_c2 = snd $
  splitGlyphs [3,4] equation

equation :: SVG
equation = scale 3 $ center $
  latexAlign "E = mc^2"

yPositions = [3,1,-1,-3]
scaleFactor = 0.7

env :: Animation -> Animation
env = addStatic bg

bg :: SVG
bg = mkBackground "lightblue"

oCenterY = oCenterXY . _2
</pre>
API references: [fork](https://hackage.haskell.org/package/reanimate-0.5.0.1/docs/Reanimate.html#v:fork), [wait](https://hackage.haskell.org/package/reanimate-0.5.0.1/docs/Reanimate.html#v:wait).









# Step 5: Drawing text

Objects are invisible by default and there are many built-in ways of showing them. Let's have a look at `oFadeIn`, `oDraw`, `oScaleIn`, and `oGrow`:

<pre class="interactive">
animation :: Animation
animation = env $ scene $ do
  fadeIn <- newText "oFadeIn"
  oModify fadeIn $ oCenterY .~ 3
  oShowWith fadeIn oFadeIn
  
  draw <- newText "oDraw"
  oModify draw $ oCenterY .~ 1
  oShowWith draw oDraw
  
  scaleIn <- newText "oScaleIn"
  oModify scaleIn $ oCenterY .~ -1
  oShowWith scaleIn $
    setDuration 1 . oScaleIn
  
  grow <- newText "oGrow"
  oModify grow $ oCenterY .~ -3
  oShowWith grow oGrow
  
  forM_ [fadeIn, draw, scaleIn, grow]
  	$ \obj -> fork $
      oHideWith obj oFadeOut

newText =
  oNew . scale 1.5 . center . latex

env :: Animation -> Animation
env = addStatic bg .
  mapA (withStrokeWidth defaultStrokeWidth) .
  mapA (withStrokeColor "black")

bg :: SVG
bg = mkBackground "lightblue"

oCenterY = oCenterXY . _2
</pre>
API references: [oFadeIn](https://hackage.haskell.org/package/reanimate-0.5.0.1/docs/Reanimate-Scene.html#v:oFadeIn), [oDraw](https://hackage.haskell.org/package/reanimate-0.5.0.1/docs/Reanimate-Scene.html#v:oDraw), [oScaleIn](https://hackage.haskell.org/package/reanimate-0.5.0.1/docs/Reanimate-Scene.html#v:oScaleIn), [oGrow](https://hackage.haskell.org/package/reanimate-0.5.0.1/docs/Reanimate-Scene.html#v:oGrow).

Let's use the `oDraw` function to draw text on the screen and let's make sure the text is right-aligned:

<pre class="interactive">
animation :: Animation
animation = env $ scene $ do
  ls <- mapM oNew [energy, equals, mass, speedOfLight]
  
  forM_ (zip ls yPositions) $
    \(obj, nth) -> do
    oModifyS obj $ do
      oLeftX .= -4
      oCenterY .= nth
    oShowWith obj oDraw

  forM_ ls $ \obj ->
    fork $ oHideWith obj oFadeOut

energy = scale 1.5 $ centerX $
  latex "Energy"

equals = scale 1.5 $ centerX $
  latex "equals"

mass = scale 1.5 $ centerX $
  latex "mass times"

speedOfLight = scale 1.5 $ centerX $
  latex "speed of light$^2$"

oCenterY = oCenterXY . _2

oTranslateY :: Lens' (ObjectData a) Double
oTranslateY = oTranslate._2

yPositions = [3,1,-1,-3]

env :: Animation -> Animation
env = addStatic (mkBackground "lightblue") .
  mapA (withStrokeWidth defaultStrokeWidth) .
  mapA (withStrokeColor "black")
</pre>


&nbsp;




# Step 6: Putting it all together

We've reached the end and have all the parts needed to re-create the GIF shown in the beginning. To recap:

 * The `Scene` monad is useful for animating a sequence of events.
 * We can typeset equations with `latex :: Text -> SVG`.
 * SVG nodes are regular Haskell data and we can extract child nodes with `splitGlyphs`.
 * Objects wrap SVG nodes and make it easy to set positioning.
 * Objects have many built-in ways of being shown on canvas.

Putting everything together gives us this short animation:

<pre class="interactive">
animation :: Animation
animation = env $ scene $ do
  symbols <- mapM oNew
    [symb_e, symb_eq, symb_m, symb_c2]
  mapM_ oShow symbols
  wait 1

  forM_ (zip symbols yPositions) $
    \(obj, yPos) -> do
    fork $ oTweenS obj 1 $ \t -> do
      oScale %= \origin -> fromToS origin scaleFactor t
      oLeftX %= \origin -> fromToS origin screenLeft t
      oCenterY %= \origin -> fromToS origin yPos t
    wait 0.3
  
  wait 1
  
  ls <- mapM oNew [energy, equals, mass, speedOfLight]
  
  forM_ (zip ls yPositions) $
    \(obj, nth) -> do
    oModifyS obj $ do
      oLeftX .= -4
      oCenterY .= nth
    oShowWith obj oDraw

  wait 2

  forM_ ls $ \obj ->
    fork $ oHideWith obj oFadeOut

  forM_ (reverse symbols) $ \obj -> do
    fork $ oTweenS obj 1 $ \t -> do
      oScale %= \origin -> fromToS origin 1 t
      (oTranslate._1) %= \pos -> fromToS pos 0 t
      (oTranslate._2) %= \pos -> fromToS pos 0 t
    wait 0.3
  wait 2

scaleFactor = 0.7

symb_e :: SVG
symb_e = snd $ splitGlyphs [0] svg

symb_eq :: SVG
symb_eq = snd $ splitGlyphs [1] svg

symb_m :: SVG
symb_m = snd $ splitGlyphs [2] svg

symb_c2 :: SVG
symb_c2 = snd $ splitGlyphs [3,4] svg

svg = scale 3 $ center $
  latexAlign "E = mc^2"

energy = scale 1.5 $ centerX $
  latex "Energy"

equals = scale 1.5 $ centerX $
  latex "equals"

mass = scale 1.5 $ centerX $
  latex "mass times"

speedOfLight = scale 1.5 $ centerX $
  latex "speed of light$^2$"

oCenterY = oCenterXY . _2

oTranslateY :: Lens' (ObjectData a) Double
oTranslateY = oTranslate._2

yPositions = [3,1,-1,-3]

env :: Animation -> Animation
env = addStatic (mkBackground "lightblue") .
  mapA (withStrokeWidth defaultStrokeWidth) .
  mapA (withStrokeColor "black")
</pre>

&nbsp;














<script>
  setTimeout(function () {
    const pres = document.querySelectorAll(".interactive");
    var delay=0;
    for(var i=0; i<pres.length; i++) {
      const elt = pres[i];
      setTimeout(function() {
        embedPlayground(elt);
      },delay);
      delay += 0;
    }
  },0);
</script>