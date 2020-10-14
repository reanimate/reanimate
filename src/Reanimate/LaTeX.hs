{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Copyright   : Written by David Himmelstrup
-- License     : Unlicense
-- Maintainer  : lemmih@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Reanimate.LaTeX
  ( latexCfg,
    TexEngine (..),
    TexConfig (..),
    latex,
    latexWithHeaders,
    latexChunks,
    xelatex,
    xelatexWithHeaders,
    ctex,
    ctexWithHeaders,
    latexAlign,

    -- * Font configurations
    chalkduster,
    calligra,
    noto,
    helvet,
    libertine,
    biolinum,
    droidSerif,
    droidSans,
  )
where

import           Control.Lens         ((&), (.~))
import qualified Data.ByteString      as B
import           Data.Hashable        (Hashable)
import           Data.Monoid          (Last (Last))
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T
import qualified Data.Text.IO         as T
import           GHC.Generics         (Generic)
import           Graphics.SvgTree     (pattern ClipPathTree, pattern None, Tree, clipPathRef,
                                       clipRule, mapTree, parseSvgFile, strokeColor)
import           Reanimate.Animation  (SVG)
import           Reanimate.Cache      (cacheDiskSvg, cacheMem)
import           Reanimate.External   (zipArchive)
import           Reanimate.Misc       (requireExecutable, runCmd, withTempDir, withTempFile)
import           Reanimate.Parameters (pNoExternals)
import           Reanimate.Svg
import           System.FilePath      (replaceExtension, takeFileName, (</>))
import           System.IO.Unsafe     (unsafePerformIO)

-- | TeX backends. They have different features and capabilities.
data TexEngine = LaTeX | XeLaTeX | LuaLaTeX
  deriving (Generic, Hashable, Eq, Ord, Read, Show)

-- | TeX configurations can load packages and set up environments for tex scripts.
data TexConfig = TexConfig
  { texConfigEngine     :: TexEngine,
    texConfigHeaders    :: [T.Text],
    texConfigPostScript :: [T.Text]
  }
  deriving (Generic, Hashable, Read, Show, Eq, Ord)

-- | Render TeX script using a given configuration.
latexCfg :: TexConfig -> T.Text -> SVG
latexCfg (TexConfig engine headers postscript) =
  gen headers postscript
  where
    gen =
      case engine of
        LaTeX    -> someTexWithHeaders engine "latex" "dvi" []
        XeLaTeX  -> someTexWithHeaders engine "xelatex" "xdv" ["-no-pdf"]
        LuaLaTeX -> someTexWithHeaders engine "lualatex" "pdf" []

-- | Invoke latex and import the result as an SVG object. SVG objects are
--   cached to improve performance.
--
--   Example:
--
--   > latex "$e^{i\\pi}+1=0$"
--
--   <<docs/gifs/doc_latex.gif>>
latex :: T.Text -> Tree
latex = latexWithHeaders []

-- | Invoke latex with extra script headers.
latexWithHeaders :: [T.Text] -> T.Text -> Tree
latexWithHeaders = someTexWithHeaders LaTeX "latex" "dvi" [] []

someTexWithHeaders ::
  TexEngine ->
  String ->
  String ->
  [String] ->
  [T.Text] ->
  [T.Text] ->
  T.Text ->
  Tree
someTexWithHeaders _engine _exec _dvi _args _headers _postscript tex
  | pNoExternals = mkText tex
someTexWithHeaders engine exec dvi args headers postscript tex =
  (unsafePerformIO . (cacheMem . cacheDiskSvg) (latexToSVG engine dvi exec args))
    script
  where
    script = mkTexScript exec args headers (T.unlines (postscript ++ [tex]))

-- | Invoke latex using a given configuration and separate results.
latexCfgChunks :: TexConfig -> [T.Text] -> [Tree]
latexCfgChunks _cfg chunks | pNoExternals = map mkText chunks
latexCfgChunks cfg chunks = worker chunks $ svgGlyphs $ tex $ T.concat chunks
  where
    tex = latexCfg cfg
    merge lst = mkGroup [fmt svg | (fmt, _, svg) <- lst]
    worker [] [] = []
    worker [] _ = error "latex chunk mismatch"
    worker (x : xs) everything =
      let width = length $ svgGlyphs (tex x)
          (first, rest) = splitAt width everything
       in merge first : worker xs rest

-- | Invoke latex and separate results.
latexChunks :: [T.Text] -> [Tree]
latexChunks = latexCfgChunks (TexConfig LaTeX [] [])

-- | Invoke xelatex and import the result as an SVG object. SVG objects are
--   cached to improve performance. Xelatex has support for non-western scripts.
xelatex :: Text -> Tree
xelatex = xelatexWithHeaders []

-- | Invoke xelatex with extra script headers.
xelatexWithHeaders :: [T.Text] -> T.Text -> Tree
xelatexWithHeaders = someTexWithHeaders XeLaTeX "xelatex" "xdv" ["-no-pdf"] []

-- | Invoke xelatex with "\usepackage[UTF8]{ctex}" and import the result as an
--   SVG object. SVG objects are cached to improve performance. Xelatex has
--   support for non-western scripts.
--
--   Example:
--
--   > ctex "中文"
--
--   <<docs/gifs/doc_ctex.gif>>
ctex :: T.Text -> Tree
ctex = ctexWithHeaders []

-- | Invoke xelatex with extra script headers + ctex headers.
ctexWithHeaders :: [T.Text] -> T.Text -> Tree
ctexWithHeaders headers = xelatexWithHeaders ("\\usepackage[UTF8]{ctex}" : headers)

-- | Invoke latex and import the result as an SVG object. SVG objects are
--   cached to improve performance. This wraps the TeX code in an 'align*'
--   context.
--
--   Example:
--
--   > latexAlign "R = \\frac{{\\Delta x}}{{kA}}"
--
--   <<docs/gifs/doc_latexAlign.gif>>
latexAlign :: Text -> Tree
latexAlign tex = latex $ T.unlines ["\\begin{align*}", tex, "\\end{align*}"]

postprocess :: Tree -> Tree
postprocess =
  simplify
    . lowerTransformations
    . scaleXY 0.1 (-0.1)
    . removeClipPaths
    . lowerIds
    . mapTree clearDrawAttr
  where
    clearDrawAttr t = t & strokeColor .~ Last Nothing

enginePostprocess :: TexEngine -> Tree -> Tree
enginePostprocess LuaLaTeX svg = translate 0 (svgHeight svg) svg
enginePostprocess _ svg        = svg

removeClipPaths :: SVG -> SVG
removeClipPaths = mapTree worker
  where
    worker ClipPathTree {} = None
    worker t               = t & clipRule .~ Last Nothing & clipPathRef .~ Last Nothing

-- executable, arguments, header, tex
latexToSVG :: TexEngine -> String -> String -> [String] -> Text -> IO Tree
latexToSVG engine dviExt latexExec latexArgs tex = do
  latexBin <- requireExecutable latexExec
  withTempDir $ \tmp_dir -> withTempFile "tex" $ \tex_file ->
    withTempFile "svg" $ \svg_file -> do
      let dvi_file =
            tmp_dir </> replaceExtension (takeFileName tex_file) dviExt
      B.writeFile tex_file (T.encodeUtf8 tex)
      runCmd
        latexBin
        ( latexArgs
            ++ [ "-interaction=nonstopmode",
                 "-halt-on-error",
                 "-output-directory=" ++ tmp_dir,
                 tex_file
               ]
        )
      if dviExt == "pdf"
        then do
          pdf2svg <- requireExecutable "pdf2svg"
          runCmd
            pdf2svg
            [dvi_file, svg_file]
        else do
          dvisvgm <- requireExecutable "dvisvgm"
          runCmd
            dvisvgm
            [ dvi_file,
              "--precision=5",
              "--exact", -- better bboxes.
              "--no-fonts", -- use glyphs instead of fonts.
              "--verbosity=0",
              "-o",
              svg_file
            ]
      svg_data <- T.readFile svg_file
      case parseSvgFile svg_file svg_data of
        Nothing -> error "Malformed svg"
        Just svg ->
          return $
            enginePostprocess engine $
              postprocess $ unbox $ replaceUses svg

mkTexScript :: String -> [String] -> [Text] -> Text -> Text
mkTexScript latexExec latexArgs texHeaders tex =
  T.unlines $
    [ "% " <> T.pack (unwords (latexExec : latexArgs)),
      "\\documentclass[preview]{standalone}",
      "\\usepackage{amsmath}",
      "\\usepackage{gensymb}"
    ]
      ++ texHeaders
      ++ [ "\\usepackage[english]{babel}",
           "\\linespread{1}",
           "\\begin{document}",
           tex,
           "\\end{document}"
         ]

{- Packages used by manim.

\\\usepackage{amsmath}\n\
\\\usepackage{amssymb}\n\
\\\usepackage{dsfont}\n\
\\\usepackage{setspace}\n\
\\\usepackage{relsize}\n\
\\\usepackage{textcomp}\n\
\\\usepackage{mathrsfs}\n\
\\\usepackage{calligra}\n\
\\\usepackage{wasysym}\n\
\\\usepackage{ragged2e}\n\
\\\usepackage{physics}\n\
\\\usepackage{xcolor}\n\
\\\usepackage{textcomp}\n\
\\\usepackage{xfrac}\n\
\\\usepackage{microtype}\n\
-}

-- | Chalkduster font. Depends on lualatex.
--   Font files are automatically downloaded.
--
-- @
-- `latexCfg` `chalkduster` "chalkduster"
-- @
--
--   <<docs/gifs/doc_chalkduster.gif>>
chalkduster :: TexConfig
chalkduster =
  TexConfig
    { texConfigEngine = XeLaTeX,
      texConfigHeaders =
        [ "\\usepackage[no-math]{fontspec}",
          "\\setmainfont[Mapping=tex-text,Path={" <> chalkdusterFont <> "/},Extension=.ttf]{Chalkduster}",
          "\\usepackage[defaultmathsizes]{mathastext}"
        ],
      texConfigPostScript = []
    }
  where
    chalkdusterFont =
      T.pack $
        zipArchive
          "https://www.ffonts.net/Chalkduster.font.zip"
          "Wplv4RjuFiI0hDQnAM5MVHl2evrZqWstRLdVAfBomCM="

-- |
-- @
-- `latexCfg` `calligra` "calligra"
-- @
--
--   <<docs/gifs/doc_calligra.gif>>
calligra :: TexConfig
calligra =
  TexConfig
    { texConfigEngine = LaTeX,
      texConfigHeaders = ["\\usepackage{calligra}"],
      texConfigPostScript = ["\\calligra"]
    }

-- |
-- @
-- `latexCfg` `noto` "noto"
-- @
--
--   <<docs/gifs/doc_noto.gif>>
noto :: TexConfig
noto =
  TexConfig
    { texConfigEngine = LaTeX,
      texConfigHeaders = ["\\usepackage{noto}"],
      texConfigPostScript = []
    }

-- |
-- @
-- `latexCfg` `helvet` "helvet"
-- @
--
--   <<docs/gifs/doc_helvet.gif>>
helvet :: TexConfig
helvet =
  TexConfig
    { texConfigEngine = LaTeX,
      texConfigHeaders = ["\\usepackage{helvet}"],
      texConfigPostScript = []
    }

-- |
-- @
-- `latexCfg` `libertine` "libertine"
-- @
--
--   <<docs/gifs/doc_libertine.gif>>
libertine :: TexConfig
libertine =
  TexConfig
    { texConfigEngine = LaTeX,
      texConfigHeaders = ["\\usepackage{libertine}"],
      texConfigPostScript = []
    }

-- |
-- @
-- `latexCfg` `biolinum` "biolinum"
-- @
--
--   <<docs/gifs/doc_biolinum.gif>>
biolinum :: TexConfig
biolinum =
  TexConfig
    { texConfigEngine = LaTeX,
      texConfigHeaders =
        ["\\usepackage{libertine}"
        ,"\\renewcommand{\\familydefault}{\\sfdefault}"],
      texConfigPostScript = []
    }

-- |
-- @
-- `latexCfg` `droidSerif` "droidSerif"
-- @
--
--   <<docs/gifs/doc_droidSerif.gif>>
droidSerif :: TexConfig
droidSerif =
  TexConfig
    { texConfigEngine = LaTeX,
      texConfigHeaders =
        ["\\usepackage[default]{droidserif}"
        ,"\\let\\varepsilon\\epsilon"],
      texConfigPostScript = []
    }

-- |
-- @
-- `latexCfg` `droidSans` "droidSans"
-- @
--
--   <<docs/gifs/doc_droidSans.gif>>
droidSans :: TexConfig
droidSans =
  TexConfig
    { texConfigEngine = LaTeX,
      texConfigHeaders =
        ["\\usepackage[default]{droidsans}"
        ,"\\let\\varepsilon\\epsilon"],
      texConfigPostScript = []
    }
