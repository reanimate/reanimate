{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Copyright   : Written by David Himmelstrup
License     : Unlicense
Maintainer  : lemmih@gmail.com
Stability   : experimental
Portability : POSIX
-}
module Reanimate.LaTeX
  ( latex
  , latexWithHeaders
  , latexChunks
  , xelatex
  , xelatexWithHeaders
  , ctex
  , ctexWithHeaders
  , latexAlign
  )
where

import qualified Data.ByteString               as B
import           Data.Text                                ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import           Graphics.SvgTree                         ( Tree
                                                          , parseSvgFile
                                                          )
import           Reanimate.Cache
import           Reanimate.Misc
import           Reanimate.Svg
import           Reanimate.Parameters
import           System.FilePath                          ( replaceExtension
                                                          , takeFileName
                                                          , (</>)
                                                          )
import           System.IO.Unsafe                         ( unsafePerformIO )

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
latexWithHeaders = someTexWithHeaders "latex" "dvi" []

someTexWithHeaders :: String -> String -> [String] -> [T.Text] -> T.Text -> Tree
someTexWithHeaders _exec _dvi _args _headers tex | pNoExternals = mkText tex
someTexWithHeaders exec dvi args headers tex =
  (unsafePerformIO . (cacheMem . cacheDiskSvg) (latexToSVG dvi exec args))
    script
 where
  script = mkTexScript exec args headers tex

-- | Invoke latex and separate results.
latexChunks :: [T.Text] -> [Tree]
latexChunks chunks | pNoExternals = map mkText chunks
latexChunks chunks                = worker (svgGlyphs $ latex $ T.concat chunks) chunks
 where
  merge lst = mkGroup [ fmt svg | (fmt, _, svg) <- lst ]
  worker [] [] = []
  worker _ [] = error "latex chunk mismatch"
  worker everything (x : xs) =
    let width = length $ svgGlyphs (latex x)
    in merge (take width everything) : worker (drop width everything) xs

-- | Invoke xelatex and import the result as an SVG object. SVG objects are
--   cached to improve performance. Xelatex has support for non-western scripts.
xelatex :: Text -> Tree
xelatex = xelatexWithHeaders []

-- | Invoke xelatex with extra script headers.
xelatexWithHeaders :: [T.Text] -> T.Text -> Tree
xelatexWithHeaders = someTexWithHeaders "xelatex" "xdv" ["-no-pdf"]

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
postprocess = simplify

-- executable, arguments, header, tex
latexToSVG :: String -> String -> [String] -> Text -> IO Tree
latexToSVG dviExt latexExec latexArgs tex = do
  latexBin <- requireExecutable latexExec
  dvisvgm  <- requireExecutable "dvisvgm"
  withTempDir $ \tmp_dir -> withTempFile "tex" $ \tex_file ->
    withTempFile "svg" $ \svg_file -> do
      let dvi_file =
            tmp_dir </> replaceExtension (takeFileName tex_file) dviExt
      B.writeFile tex_file (T.encodeUtf8 tex)
      runCmd
        latexBin
        (  latexArgs
        ++ [ "-interaction=nonstopmode"
           , "-halt-on-error"
           , "-output-directory=" ++ tmp_dir
           , tex_file
           ]
        )
      runCmd
        dvisvgm
        [ dvi_file
        , "--precision=5"
        , "--exact"    -- better bboxes.
        , "--no-fonts" -- use glyphs instead of fonts.
        , "--scale=0.1,-0.1"
        , "--verbosity=0"
        , "-o"
        , svg_file
        ]
      svg_data <- B.readFile svg_file
      case parseSvgFile svg_file svg_data of
        Nothing  -> error "Malformed svg"
        Just svg -> return $ postprocess $ unbox $ replaceUses svg

mkTexScript :: String -> [String] -> [Text] -> Text -> Text
mkTexScript latexExec latexArgs texHeaders tex =
  T.unlines
    $  [ "% " <> T.pack (unwords (latexExec : latexArgs))
       , "\\documentclass[preview]{standalone}"
       , "\\usepackage{amsmath}"
       , "\\usepackage{gensymb}"
       ]
    ++ texHeaders
    ++ [ "\\usepackage[english]{babel}"
       , "\\linespread{1}"
       , "\\begin{document}"
       , tex
       , "\\end{document}"
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
