{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reanimate.LaTeX (latex,xelatex,latexAlign) where

import qualified Data.ByteString   as B
import           Data.Monoid ((<>))
import           Data.Text         (Text)
import qualified Data.Text         as T
import qualified Data.Text.IO      as T
import           Graphics.SvgTree  (Tree (..), parseSvgFile)
import           Reanimate.Cache
import           Reanimate.Misc
import           Reanimate.Svg
import           Reanimate.Parameters
import           System.FilePath   (replaceExtension, takeFileName, (</>))
import           System.IO.Unsafe  (unsafePerformIO)

-- | Invoke latex and import the result as an SVG object. SVG objects are
--   cached to improve performance.
--
--   Example:
--
--   > latex "$e^{i\\pi}+1=0$"
--
--   <<docs/gifs/doc_latex.gif>>
latex :: T.Text -> Tree
latex tex | pNoExternals = mkText tex
latex tex = (unsafePerformIO . (cacheMem . cacheDiskSvg) (latexToSVG "dvi" exec args)) script
  where
    exec = "latex"
    args = []
    script = mkTexScript exec args [] tex

-- | Invoke xelatex and import the result as an SVG object. SVG objects are
--   cached to improve performance. Xelatex has support for non-western scripts.
--
--   Example:
--
--   > xelatex "中文"
--
--   <<docs/gifs/doc_xelatex.gif>>
xelatex :: Text -> Tree
xelatex tex | pNoExternals = mkText tex
xelatex tex = (unsafePerformIO . (cacheMem . cacheDiskSvg) (latexToSVG "xdv" exec args)) script
  where
    exec = "xelatex"
    args = ["-no-pdf"]
    headers = ["\\usepackage[UTF8]{ctex}"]
    script = mkTexScript exec args headers tex

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
postprocess = lowerTransformations . scaleXY 1 (-1) . scale 0.1 . pathify

-- executable, arguments, header, tex
latexToSVG :: String -> String -> [String] -> Text -> IO Tree
latexToSVG dviExt latexExec latexArgs tex = do
  latexBin <- requireExecutable latexExec
  dvisvgm <- requireExecutable "dvisvgm"
  withTempDir $ \tmp_dir -> withTempFile "tex" $ \tex_file -> withTempFile "svg" $ \svg_file -> do
    let dvi_file = tmp_dir </> replaceExtension (takeFileName tex_file) dviExt
    T.writeFile tex_file tex
    runCmd latexBin (latexArgs ++ ["-interaction=nonstopmode", "-halt-on-error", "-output-directory="++tmp_dir, tex_file])
    runCmd dvisvgm [ dvi_file, "--precision=5"
                   , "--exact"    -- better bboxes.
                   -- , "--bbox=1,1" -- increase bbox size.
                   , "--no-fonts" -- use glyphs instead of fonts.
                   ,"--verbosity=0", "-o",svg_file]
    svg_data <- B.readFile svg_file
    case parseSvgFile svg_file svg_data of
      Nothing  -> error "Malformed svg"
      Just svg -> return $ postprocess $ unbox $ replaceUses svg

mkTexScript :: String -> [String] -> [Text] -> Text -> Text
mkTexScript latexExec latexArgs texHeaders tex = T.unlines $
  [ "% " <> T.pack (unwords (latexExec:latexArgs))
  , "\\documentclass[preview]{standalone}"
  , "\\usepackage{amsmath}"
  , "\\usepackage{gensymb}"
  ] ++ texHeaders ++
  [ "\\usepackage[english]{babel}"
  , "\\linespread{1}"
  , "\\begin{document}"
  , tex
  , "\\end{document}" ]

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
