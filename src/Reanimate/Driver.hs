module Reanimate.Driver ( reanimate ) where

import           Control.Concurrent           (MVar, forkIO, killThread,
                                               modifyMVar_, newEmptyMVar,
                                               putMVar, takeMVar)
import           Control.Exception            (finally)
import           Control.Monad.Fix            (fix)
import           Control.Monad
import qualified Data.Text                    as T
import qualified Data.Text.Read               as T
import           Network.WebSockets
import           System.Directory             (findFile, listDirectory, findExecutable)
import           System.Environment           (getArgs, getProgName)
import           System.Exit
import Text.Printf
import           System.FilePath
import           System.FSNotify
import           System.IO                    (BufferMode (..), hPutStrLn,
                                               hSetBuffering, stderr, stdin)
import qualified Text.PrettyPrint.ANSI.Leijen as Doc

import           Data.Maybe
import           Paths_reanimate
import           Reanimate.Misc               (runCmdLazy, runCmd_, withTempDir,
                                               withTempFile)
import           Reanimate.Monad              (Animation)
import           Reanimate.Render             (render, renderSnippets,
                                               renderSvgs)
import           Web.Browser                  (openBrowser)

opts = defaultConnectionOptions
  { connectionCompressionOptions = PermessageDeflateCompression defaultPermessageDeflate }

reanimate :: Animation -> IO ()
reanimate animation = do
  watch <- startManager
  args <- getArgs
  hSetBuffering stdin NoBuffering
  case args of
    ["once"] -> renderSvgs animation
    ["snippets"] -> renderSnippets animation
    ["check"] -> checkEnvironment
    ["render", target] ->
      render animation target
    _ -> withTempDir $ \tmpDir -> do
      url <- getDataFileName "viewer/build/index.html"
      putStrLn "Opening browser..."
      bSucc <- openBrowser url
      if bSucc
          then putStrLn "Browser opened."
          else hPutStrLn stderr $ "Failed to open browser. Manually visit: " ++ url
      runServerWith "127.0.0.1" 9161 opts $ \pending -> do
        putStrLn "Server pending..."
        prog <- getProgName
        lst <- listDirectory "."
        mbSelf <- findFile ("." : lst) prog
        blocker <- newEmptyMVar :: IO (MVar ())
        case mbSelf of
          Nothing -> do
            hPutStrLn stderr "Failed to find own source code."
          Just self -> do
            conn <- acceptRequest pending
            slave <- newEmptyMVar
            let handler = modifyMVar_ slave $ \tid -> do
                  sendTextData conn (T.pack "Compiling")
                  putStrLn "Killing and respawning..."
                  killThread tid
                  tid <- forkIO $ slaveHandler conn self tmpDir
                  return tid
                killSlave = do
                  tid <- takeMVar slave
                  killThread tid
            putStrLn "Found self. Listening..."
            stop <- watchFile watch self handler
            putMVar slave =<< forkIO (return ())
            let loop = do
                  fps <- receiveData conn :: IO T.Text
                  handler
                  loop
            loop `finally` (killSlave >> stop)

slaveHandler conn self tmpDir = withTempFile ".exe" $ \tmpExecutable -> do
  ret <- runCmd_ "stack" $ ["ghc", "--"] ++ ghcOptions tmpDir ++ [self, "-o", tmpExecutable]
  case ret of
    Left err ->
      sendTextData conn $ T.pack $ "Error" ++ unlines (drop 3 (lines err))
    Right{} -> do
      getFrame <- runCmdLazy tmpExecutable ["once", "+RTS", "-N", "-M1G", "-RTS"]
      (frameCount,_) <- expectFrame =<< getFrame
      -- sendTextData conn (T.pack "Compiled")
      sendTextData conn (T.pack $ show frameCount)
      fix $ \loop -> do
        (frameIdx, frame) <- expectFrame =<< getFrame
        sendTextData conn (T.pack $ show frameIdx)
        sendTextData conn frame
        loop
  where
    expectFrame (Left "") = do
      sendTextData conn (T.pack "Done")
      exitWith ExitSuccess
    expectFrame (Left err) = do
      sendTextData conn $ T.pack $ "Error" ++ err
      exitWith (ExitFailure 1)
    expectFrame (Right frame) =
      case T.decimal frame of
        Left err -> do
          hPutStrLn stderr (T.unpack frame)
          hPutStrLn stderr $ "expectFrame: " ++ err
          sendTextData conn $ T.pack $ "Error" ++ err
          exitWith (ExitFailure 1)
        Right (frameNumber, rest) -> pure (frameNumber, rest)

watchFile watch file action = watchDir watch (takeDirectory file) check (const action)
  where
    check event = takeFileName (eventPath event) == takeFileName file

ghcOptions :: FilePath -> [String]
ghcOptions tmpDir =
    ["-rtsopts", "--make", "-threaded", "-O2"] ++
    ["-odir", tmpDir, "-hidir", tmpDir]




--------------------------------------------------------------------------
-- Check environment

checkEnvironment :: IO ()
checkEnvironment = do
    putStrLn "reanimate checks:"
    runCheck "Has LaTeX" hasLaTeX
    runCheck "Has XeLaTeX" hasXeLaTeX
    runCheck "Has dvisvgm" hasDvisvgm
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

hasTeXPackage :: FilePath -> String -> IO (Either String String)
hasTeXPackage exec pkg =
    withTempDir $ \tmp_dir -> withTempFile "tex" $ \tex_file -> do
      let tmp_dir = "."
          tex_file = "test.tex"
      writeFile tex_file tex_document
      appendFile tex_file $ "\\usepackage" ++ pkg ++ "\n"
      appendFile tex_file "\\begin{document}\n"
      appendFile tex_file "blah\n"
      appendFile tex_file tex_epilogue
      ret <- runCmd_ exec ["-interaction=batchmode", "-halt-on-error", "-output-directory="++tmp_dir, tex_file]
      return $ case ret of
        Right{} -> Right "OK"
        Left{}  -> Left "missing"
  where
    tex_document = "\\documentclass[preview]{standalone}\n"
    tex_xelatex =
      "\\usepackage[UTF8]{ctex}\n"

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

    tex_epilogue =
      "\n\
      \\\end{document}"

hasProgram :: String -> IO (Either String String)
hasProgram exec = do
  mbPath <- findExecutable exec
  return $ case mbPath of
    Nothing   -> Left $ "'" ++ exec ++ "'' not found"
    Just path -> Right path
