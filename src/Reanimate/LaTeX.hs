{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reanimate.LaTeX (latex,xelatex,latexAlign) where

import           Control.Exception (SomeException, handle)
import qualified Data.ByteString   as B
import           Data.Monoid ((<>))
import           Data.Text         (Text)
import qualified Data.Text         as T
import qualified Data.Text.IO      as T
import           Graphics.SvgTree  (Tree (..), defaultSvg, parseSvgFile)
import           Reanimate.Cache
import           Reanimate.Misc
import           Reanimate.Svg
import           System.FilePath   (replaceExtension, takeFileName, (</>))
import           System.IO.Unsafe  (unsafePerformIO)

latex :: T.Text -> Tree
latex tex = (unsafePerformIO . (cacheMem . cacheDiskSvg) latexToSVG)
  ("% plain latex\n" <> tex)

xelatex :: Text -> Tree
xelatex tex = (unsafePerformIO . (cacheMem . cacheDiskSvg) xelatexToSVG)
  ("% xelatex\n" <> tex)

latexAlign :: Text -> Tree
latexAlign tex = latex $ T.unlines ["\\begin{align*}", tex, "\\end{align*}"]

postprocess :: Tree -> Tree
postprocess = lowerTransformations . scaleXY 1 (-1) . scale 0.1 . pathify

latexToSVG :: Text -> IO Tree
latexToSVG tex = handle (\(_::SomeException) -> return (failedSvg tex)) $ do
  latexBin <- requireExecutable "latex"
  dvisvgm <- requireExecutable "dvisvgm"
  withTempDir $ \tmp_dir -> withTempFile "tex" $ \tex_file -> withTempFile "svg" $ \svg_file -> do
    let dvi_file = tmp_dir </> replaceExtension (takeFileName tex_file) "dvi"
    writeFile tex_file tex_document
    appendFile tex_file tex_prologue
    T.appendFile tex_file tex
    appendFile tex_file tex_epilogue
    runCmd latexBin ["-interaction=batchmode", "-halt-on-error", "-output-directory="++tmp_dir, tex_file]
    runCmd dvisvgm [ dvi_file
                   , "--exact"    -- better bboxes.
                   -- , "--bbox=1,1" -- increase bbox size.
                   , "--no-fonts" -- use glyphs instead of fonts.
                   ,"--verbosity=0", "-o",svg_file]
    svg_data <- B.readFile svg_file
    case parseSvgFile svg_file svg_data of
      Nothing  -> error "Malformed svg"
      Just svg -> return $ postprocess $ unbox $ replaceUses svg

xelatexToSVG :: Text -> IO Tree
xelatexToSVG tex = handle (\(_::SomeException) -> return (failedSvg tex)) $ do
  xetexBin <- requireExecutable "xelatex"
  dvisvgm <- requireExecutable "dvisvgm"
  withTempDir $ \tmp_dir -> withTempFile "tex" $ \tex_file -> withTempFile "svg" $ \svg_file -> do
    let dvi_file = tmp_dir </> replaceExtension (takeFileName tex_file) "xdv"
    writeFile tex_file tex_document
    appendFile tex_file tex_xelatex
    appendFile tex_file tex_prologue
    T.appendFile tex_file tex
    appendFile tex_file tex_epilogue
    runCmd xetexBin ["-no-pdf", "-interaction=batchmode", "-halt-on-error", "-output-directory="++tmp_dir, tex_file]
    runCmd dvisvgm [ dvi_file
                   , "--exact"    -- better bboxes.
                   -- , "--bbox=1,1" -- increase bbox size.
                   , "--no-fonts" -- use glyphs instead of fonts.
                   ,"--verbosity=0", "-o",svg_file]
    svg_data <- B.readFile svg_file
    case parseSvgFile svg_file svg_data of
      Nothing  -> error "Malformed svg"
      Just svg -> return $ postprocess $ unbox $ replaceUses svg

failedSvg :: Text -> Tree
failedSvg _tex = defaultSvg
  -- text_ [ font_size_ "20"
  --       , fill_ "white"] (toHtml $ "bad latex: "++tex)

tex_document :: String
tex_document = "\\documentclass[preview]{standalone}\n"

tex_xelatex :: String
tex_xelatex =
  "\\usepackage[UTF8]{ctex}\n"

tex_prologue :: String
tex_prologue =
  "\\usepackage[english]{babel}\n\
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
  \\\linespread{1}\n\
  \\\begin{document}\n"

tex_epilogue :: String
tex_epilogue =
  "\n\
  \\\end{document}"
