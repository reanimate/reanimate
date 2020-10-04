# System fonts


<pre class="interactive">
magnolia =
  TexConfig
    { texConfigEngine = XeLaTeX,
      texConfigHeaders =
        [ "\\usepackage[no-math]{fontspec}",
          "\\setmainfont[Mapping=tex-text,Path={" <> magnoliaFont <> "/},Extension=.otf]{Magnolia Script}",
          "\\usepackage[defaultmathsizes]{mathastext}"
        ],
      texConfigPostScript = []
    }
  where
    magnoliaFont =
      T.pack $
        zipArchive
          "https://dl.dafont.com/dl/?f=magnolia_script"
          "missing"

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