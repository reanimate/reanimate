{-# LANGUAGE ScopedTypeVariables #-}
module Reanimate.Driver.Check
  ( checkEnvironment
  ) where

import           Control.Exception            (SomeException, handle)
import           Control.Monad
import           Data.Maybe
import           Data.Version
import           Reanimate.Misc               (runCmd_)
import           System.Directory             (findExecutable)
import           System.IO
import           System.IO.Temp
import           Text.ParserCombinators.ReadP
import qualified Text.PrettyPrint.ANSI.Leijen as Doc
import           Text.Printf

--------------------------------------------------------------------------
-- Check environment

checkEnvironment :: IO ()
checkEnvironment = do
    putStrLn "reanimate checks:"
    runCheck "Has ffmpeg" hasFFmpeg
    runCheck "Has LaTeX" hasLaTeX
    runCheck "Has XeLaTeX" hasXeLaTeX
    runCheck "Has dvisvgm" hasDvisvgm
    runCheck "Has povray" hasPovray
    forM_ latexPackages $ \pkg ->
      runCheck ("Has LaTeX package '"++ pkg ++ "'") $ hasTeXPackage "latex" $
        "{"++pkg++"}"
    forM_ xelatexPackages $ \pkg ->
      runCheck ("Has XeLaTeX package '"++ pkg ++ "'") $ hasTeXPackage "xelatex" $
        "{"++pkg++"}"
  where
    latexPackages =
      ["babel"
      ,"amsmath"
      ,"amssymb"
      ,"dsfont"
      ,"setspace"
      ,"relsize"
      ,"textcomp"
      ,"mathrsfs"
      ,"calligra"
      ,"wasysym"
      ,"ragged2e"
      ,"physics"
      ,"xcolor"
      ,"textcomp"
      ,"xfrac"
      ,"microtype"]
    xelatexPackages =
      ["ctex"]
    runCheck msg fn = do
      printf "  %-35s" (msg ++ ":")
      val <- fn
      case val of
        Left err -> print $ Doc.red $ Doc.text err
        Right ok -> print $ Doc.green $ Doc.text ok

-- latex, dvisvgm, xelatex

hasLaTeX :: IO (Either String String)
hasLaTeX = hasProgram "latex"

hasXeLaTeX :: IO (Either String String)
hasXeLaTeX = hasProgram "xelatex"

hasDvisvgm :: IO (Either String String)
hasDvisvgm = hasProgram "dvisvgm"

hasPovray :: IO (Either String String)
hasPovray = hasProgram "povray"

hasFFmpeg :: IO (Either String String)
hasFFmpeg = do
  mbVersion <- ffmpegVersion
  return $ case mbVersion of
    Nothing                   -> Left "no"
    Just vs | vs < minVersion -> Left "too old"
            | otherwise       -> Right (showVersion vs)
  where
    minVersion = Version [4,1,3] []

ffmpegVersion :: IO (Maybe Version)
ffmpegVersion = do
  mbPath <- findExecutable "ffmpeg"
  case mbPath of
    Nothing   -> return Nothing
    Just path -> do
      ret <- runCmd_ path ["-version"]
      case ret of
        Left{} -> return $ Just noVersion
        Right out ->
          case map (take 3 . words) $ take 1 $ lines out of
            [["ffmpeg", "version", vs]] ->
              return $ parseVS vs
            _ -> return $ Just noVersion
  where
    noVersion = Version [] []
    parseVS vs = listToMaybe
      [ v | (v, "") <- readP_to_S parseVersion vs ]


hasTeXPackage :: FilePath -> String -> IO (Either String String)
hasTeXPackage exec pkg = handle (\(_::SomeException) -> return $ Left "n/a") $
    withSystemTempDirectory "reanimate" $ \tmp_dir -> withTempFile tmp_dir "test.tex" $ \tex_file tex_handle -> do
      hPutStr tex_handle tex_document
      hPutStr tex_handle $ "\\usepackage" ++ pkg ++ "\n"
      hPutStr tex_handle "\\begin{document}\n"
      hPutStr tex_handle "blah\n"
      hPutStr tex_handle tex_epilogue
      hClose tex_handle
      ret <- runCmd_ exec ["-interaction=batchmode", "-halt-on-error", "-output-directory="++tmp_dir, tex_file]
      return $ case ret of
        Right{} -> Right "OK"
        Left{}  -> Left "missing"
  where
    tex_document = "\\documentclass[preview]{standalone}\n"
    tex_epilogue =
      "\n\
      \\\end{document}"

hasProgram :: String -> IO (Either String String)
hasProgram exec = do
  mbPath <- findExecutable exec
  return $ case mbPath of
    Nothing   -> Left $ "'" ++ exec ++ "'' not found"
    Just path -> Right path
