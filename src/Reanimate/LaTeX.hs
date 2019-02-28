{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reanimate.LaTeX (latex,xelatex,latexAlign) where

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

import           Control.Lens          (over, set, (%~), (&), (.~), (^.))
import           Graphics.Svg          (Document (..), Tree (..), defaultSvg,
                                        elements, loadSvgFile, parseSvgFile,
                                        xmlOfDocument)
import           Text.XML.Light        (elContent)
import           Text.XML.Light.Output (ppcContent, ppcElement, prettyConfigPP)

-- instance ToHtml Document where
--   toHtml = toHtmlRaw
--   toHtmlRaw = toHtmlRaw . ppcElement prettyConfigPP . xmlOfDocument

instance ToHtml Document where
  toHtml = toHtmlRaw
  toHtmlRaw doc = toHtmlRaw $ unlines $ map (ppcContent prettyConfigPP) (elContent elt)
    where
      elt = xmlOfDocument doc

instance ToHtml Tree where
  toHtml = toHtmlRaw
  toHtmlRaw tree = toHtmlRaw doc
    where
      doc = Document
        { _viewBox = Nothing
        , _width = Nothing
        , _height = Nothing
        , _elements = [tree]
        , _definitions = Map.empty
        , _description = ""
        , _styleRules = []
        , _documentLocation = ""
        }

{-# NOINLINE cache #-}
cache :: IORef (Map String Tree)
cache = unsafePerformIO (newIORef Map.empty)

latex :: String -> Tree
latex tex = unsafePerformIO $ do
  store <- readIORef cache
  case Map.lookup tex store of
    Just svg -> return svg
    Nothing -> do
      svg <- latexToSVG tex
      case svg of
        None -> pure None
        _ -> atomicModifyIORef cache (\store -> (Map.insert tex svg store, svg))

xelatex :: String -> Tree
xelatex tex = unsafePerformIO $ do
  store <- readIORef cache
  case Map.lookup tex store of
    Just svg -> return svg
    Nothing -> do
      svg <- xelatexToSVG tex
      case svg of
        None -> pure None
        _ -> atomicModifyIORef cache (\store -> (Map.insert tex svg store, svg))

latexAlign :: String -> Tree
latexAlign tex = latex $ unlines ["\\begin{align*}", tex, "\\end{align*}"]


latexToSVG :: String -> IO Tree
latexToSVG tex = handle (\(e::SomeException) -> return (failedSvg tex)) $ do
  latex <- requireExecutable "latex"
  dvisvgm <- requireExecutable "dvisvgm"
  withTempDir $ \tmp_dir -> withTempFile "tex" $ \tex_file -> withTempFile "svg" $ \svg_file -> do
    let dvi_file = tmp_dir </> replaceExtension (takeFileName tex_file) "dvi"
    writeFile tex_file tex_document
    appendFile tex_file tex_prologue
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
      Nothing  -> error "Malformed svg"
      Just svg -> return $ unbox $ replaceUses svg

xelatexToSVG :: String -> IO Tree
xelatexToSVG tex = handle (\(e::SomeException) -> return (failedSvg tex)) $ do
  latex <- requireExecutable "xelatex"
  dvisvgm <- requireExecutable "dvisvgm"
  withTempDir $ \tmp_dir -> withTempFile "tex" $ \tex_file -> withTempFile "svg" $ \svg_file -> do
    let dvi_file = tmp_dir </> replaceExtension (takeFileName tex_file) "xdv"
    writeFile tex_file tex_document
    appendFile tex_file tex_xelatex
    appendFile tex_file tex_prologue
    appendFile tex_file tex
    appendFile tex_file tex_epilogue
    runCmd latex ["-no-pdf", "-interaction=batchmode", "-halt-on-error", "-output-directory="++tmp_dir, tex_file]
    runCmd dvisvgm [ dvi_file
                   , "--exact"    -- better bboxes.
                   -- , "--bbox=1,1" -- increase bbox size.
                   , "--no-fonts" -- use glyphs instead of fonts.
                   ,"--verbosity=0", "-o",svg_file]
    svg_data <- B.readFile svg_file
    case parseSvgFile svg_file svg_data of
      Nothing  -> error "Malformed svg"
      Just svg -> return $ unbox $ replaceUses svg

failedSvg :: String -> Tree
failedSvg tex = defaultSvg
  -- text_ [ font_size_ "20"
  --       , fill_ "white"] (toHtml $ "bad latex: "++tex)

tex_document = "\\documentclass[preview]{standalone}\n"
tex_xelatex =
  "\\usepackage[UTF8]{ctex}\n"
tex_prologue =
  "\\usepackage[english]{babel}\n\
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
  \\\usepackage{xfrac}\n\
  \\\usepackage{microtype}\n\
  \\\linespread{1}\n\
  \\\begin{document}\n"

tex_epilogue =
  "\n\
  \\\end{document}"
