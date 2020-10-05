# OTF and TTF fonts

Fonts files are often found in zip archives or tarballs and Reanimate can download them for you. Let's look at [Magnolia Script](https://www.dafont.com/magnolia-script.font) as an example. It's a free OTF font, available for download in a zip-file: <https://dl.dafont.com/dl/?f=magnolia_script>

We'll use the `zipArchive` function for downloading and unpacking the file. For safety reasons, we have to specify the SHA256 hash of the file we're downloading. This prevents unexpected changes. We can use an incorrect hash value to find the real checksum:

```haskell
magnoliaFont :: FilePath
magnoliaFont = zipArchive
  "https://dl.dafont.com/dl/?f=magnolia_script"
  "missing"
```

When we use an incorrect hash value, we'll get an error message like this:

```text
Exception: URL https://dl.dafont.com/dl/?f=magnolia_script
  Expected SHA256: missing
  Actual SHA256:   XeXawkqqnSPgxK7G72RdL39ddKPrrLPCwJB7dojuulc=
```

Copy-pasting the actual SHA256 value gives us the proper code for downloading the Magnolia Script font:

```haskell
magnoliaFont :: FilePath
magnoliaFont = zipArchive
  "https://dl.dafont.com/dl/?f=magnolia_script"
  "XeXawkqqnSPgxK7G72RdL39ddKPrrLPCwJB7dojuulc="
```

Next we'll use the font folder in a TeX configuration:

```haskell
magnolia = TexConfig {
  texConfigEngine = XeLaTeX,
  texConfigHeaders =
    [ "\\usepackage[no-math]{fontspec}",
      "\\setmainfont[\
        \Mapping=tex-text,\
        \Path={" <> T.pack magnoliaFont <> "/},\
        \Extension=.otf]\
      \{Magnolia Script}"
    ],
  texConfigPostScript = [] }
```

The `magnolia` configuration is ready for formatting text:

<pre class="interactive">
magnoliaFont :: FilePath
magnoliaFont = zipArchive
  "https://dl.dafont.com/dl/?f=magnolia_script"
  "XeXawkqqnSPgxK7G72RdL39ddKPrrLPCwJB7dojuulc="

magnolia = TexConfig {
  texConfigEngine = XeLaTeX,
  texConfigHeaders =
    [ "\\usepackage[no-math]{fontspec}",
      "\\setmainfont[\
      \Mapping=tex-text,\
      \Path={" <>
      T.pack magnoliaFont <>
      "/},\
      \Extension=.otf]\
      \{Magnolia Script}"
    ],
  texConfigPostScript = [] }

animation :: Animation
animation = scene $
  showCfg "Magnolia" magnolia

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