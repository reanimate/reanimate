# Font Catalogue

The [$\LaTeX$ font catalogue](https://tug.org/FontCatalogue/) is a listing of a wide variety of fonts with examples. Let's explore how to use those fonts in Reanimate.

## Fetamont

Go to <https://tug.org/FontCatalogue/fetamont/> and look under the `Usage` section. There you can find $\LaTeX$ preamble code and style selector code. The preamble looks like this:
```
\usepackage{fetamont}
\usepackage[T1]{fontenc}
```
And the style selector looks like this: `\ffmfamily`

Armed with this information, we can create a TeX configuration in Reanimate:

<pre class="interactive">
fetamont = TexConfig LaTeX
  ["\\usepackage{fetamont}"
  ,"\\usepackage[T1]{fontenc}"]
  ["\\ffmfamily"]

animation :: Animation
animation = scene $
  showCfg "Fetamont" fetamont

{!docs/font_script.hs!}
</pre>

<br/>

## Inconsolata

Configuration code for Inconsolata can be found here: <https://tug.org/FontCatalogue/inconsolata/>

<pre class="interactive">
inconsolata = TexConfig LaTeX
  ["\\usepackage{inconsolata}"
  ,"\\renewcommand*\\familydefault{\\ttdefault}"
  ,"\\usepackage[T1]{fontenc}"]
  ["\\normalfont"]

animation :: Animation
animation = scene $
  showCfg "Inconsolata" inconsolata

{!docs/font_script.hs!}
</pre>

<br/>

## Computer Modern

[Computer modern](https://tug.org/FontCatalogue/computermodern/) is the default font. It's presented here as a reference and as a point of comparison.

<pre class="interactive">
-- Default font so no need to do anything.
computerModern = TexConfig LaTeX
  []
  []

animation :: Animation
animation = scene $
  showCfg "Computer Modern" computerModern

{!docs/font_script.hs!}
</pre>

<br/>

## Missing fonts

Fonts only work if they are installed in your TeX setup. If a font is missing, you'll get a cryptic error message like this:

<pre class="interactive">
clayTablet = TexConfig LaTeX
  ["\\usepackage{fontspec}"
  ,"\\setmainfont{QTClaytablet}"]
  ["\\normalfont"]

animation :: Animation
animation = scene $
  showCfg "Claytablet" clayTablet

{!docs/font_script.hs!}
</pre>

<br/>

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