{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reanimate.LaTeX (latex) where

import           Control.Exception     (SomeException, handle)
import qualified Data.ByteString       as B
import           Data.IORef
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Lucid                 (ToHtml (..))
import           Lucid.Svg             (Svg, fill_, font_size_, text_)
import           Reanimate.Misc
import           Reanimate.Svg
import           System.FilePath       (replaceExtension, takeFileName, (</>))
import           System.IO.Unsafe      (unsafePerformIO)

import           Graphics.Svg          (loadSvgFile, parseSvgFile,
                                        xmlOfDocument, Tree, elements, defaultSvg, Document)
import           Text.XML.Light.Output (ppcElement, ppcContent, prettyConfigPP)
import           Text.XML.Light (elContent)
import Control.Lens (over, (^.),set, (.~), (&), (%~) )

-- instance ToHtml Document where
--   toHtml = toHtmlRaw
--   toHtmlRaw = toHtmlRaw . ppcElement prettyConfigPP . xmlOfDocument

instance ToHtml Document where
  toHtml = toHtmlRaw
  toHtmlRaw doc = toHtmlRaw $ unlines $ map (ppcContent prettyConfigPP) (elContent elt)
    where
      elt = xmlOfDocument doc

{-# NOINLINE cache #-}
cache :: IORef (Map String (Svg ()))
cache = unsafePerformIO (newIORef Map.empty)

latex :: String -> Svg ()
latex tex = unsafePerformIO $ do
  store <- readIORef cache
  case Map.lookup tex store of
    Just svg -> return svg
    Nothing -> do
      svg <- latexToSVG tex
      atomicModifyIORef cache (\store -> (Map.insert tex svg store, svg))


latexToSVG :: String -> IO (Svg ())
latexToSVG tex = handle (\(e::SomeException) -> return (failedSvg tex)) $ do
  latex <- requireExecutable "latex"
  dvisvgm <- requireExecutable "dvisvgm"
  withTempDir $ \tmp_dir -> withTempFile "tex" $ \tex_file -> withTempFile "svg" $ \svg_file -> do
    let dvi_file = tmp_dir </> replaceExtension (takeFileName tex_file) "dvi"
    writeFile tex_file tex_prologue
    appendFile tex_file tex
    appendFile tex_file tex_epilogue
    runCmd latex ["-interaction=batchmode", "-halt-on-error", "-output-directory="++tmp_dir, tex_file]
    runCmd dvisvgm [ dvi_file
                   , "--exact"    -- better bboxes.
                   -- , "--bbox=1,1" -- increase bbox size.
                   , "--no-fonts" -- use glyphs instead of fonts.
                   ,"--verbosity=0", "-o",svg_file]
    svg_data <- B.readFile svg_file
    case parseSvgFile svg_file svg_data of
      Nothing -> error "Malformed svg"
      Just svg -> return $ toHtmlRaw $ unbox $ replaceUses svg

failedSvg :: String -> Svg ()
failedSvg tex =
  text_ [ font_size_ "20"
        , fill_ "white"] (toHtml $ "bad latex: "++tex)

tex_prologue =
  "\\documentclass[preview]{standalone}\n\
  \\\usepackage[english]{babel}\n\
  \\\usepackage{amsmath}\n\
  \\\usepackage{amssymb}\n\
  \\\usepackage{dsfont}\n\
  \\\usepackage{setspace}\n\
  \\\usepackage{tipa}\n\
  \\\usepackage{relsize}\n\
  \\\usepackage{textcomp}\n\
  \\\usepackage{mathrsfs}\n\
  \\\usepackage{calligra}\n\
  \\\usepackage{wasysym}\n\
  \\\usepackage{ragged2e}\n\
  \\\usepackage{physics}\n\
  \\\usepackage{xcolor}\n\
  \\\usepackage{textcomp}\n\
  \\\usepackage{microtype}\n\
  \\\DisableLigatures{encoding = *, family = * }\n\
  \%\\usepackage[UTF8]{ctex}\n\
  \\\linespread{1}\n\
  \\\begin{document}\n\
  \\\begin{align*}\n"

tex_epilogue =
  "\n\
  \\\end{align*}\n\
  \\\end{document}"
