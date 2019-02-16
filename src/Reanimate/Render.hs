module Reanimate.Render
  ( render
  ) where

import           Control.Exception  (evaluate, finally, throwIO)
import           Control.Monad      (forM_)
import           Lucid.Svg          (renderToFile)
import           Reanimate.Arrow    (Ani, animationDuration, frameAt)
import           Reanimate.Examples
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.IO
import           System.Process
import           Text.Printf        (printf)


data Format = RenderMp4 | RenderGif | RenderWebm

formatFPS :: Format -> Int
formatFPS RenderMp4  = 60
formatFPS RenderGif  = 25
formatFPS RenderWebm = 30

render :: Ani () -> FilePath -> IO ()
render ani target =
  case takeExtension target of
    ".mp4"  -> renderFormat RenderMp4 ani target
    ".gif"  -> renderFormat RenderGif ani target
    ".webm" -> renderFormat RenderWebm ani target
    ext     -> error $ "Unknown media format: " ++ show ext

renderFormat :: Format -> Ani () -> FilePath -> IO ()
renderFormat format ani target = do
  putStrLn $ "Starting render of animation: " ++ show (round (animationDuration ani)) ++ "s"
  ffmpeg <- requireExecutable "ffmpeg"
  generateFrames ani fps $ \template ->
    case format of
      RenderMp4 ->
        runCmd ffmpeg ["-r", show fps, "-i", template, "-y"
                      , "-c:v", "libx264", "-vf", "fps="++show fps
                      , "-pix_fmt", "yuv420p", target]
      RenderGif -> withTempFile "png" $ \palette -> do
        runCmd ffmpeg ["-i", template, "-y"
                      ,"-vf", "fps="++show fps++",scale=320:-1:flags=lanczos,palettegen"
                      ,"-t", show (animationDuration ani)
                      , palette ]
        runCmd ffmpeg ["-i", template, "-y"
                      ,"-i", palette
                      ,"-filter_complex"
                      ,"fps="++show fps++",scale=320:-1:flags=lanczos[x];[x][1:v]paletteuse"
                      ,"-t", show (animationDuration ani)
                      , target]
      RenderWebm ->
        runCmd ffmpeg ["-r", show fps, "-i", template, "-y"
                      , "-c:v", "libvpx-vp9", "-vf", "fps="++show fps
                      , target]
  where
    fps = formatFPS format

---------------------------------------------------------------------------------
-- Helpers
-- XXX: Move to a different module and unify with helpers from LaTeX.

-- XXX: Use threads
generateFrames ani rate action = withTempDir $ \tmp -> do
    let frameName nth = tmp </> printf nameTemplate nth
    forM_ frames $ \n ->
      renderToFile (frameName n) (nthFrame n)
    action (tmp </> nameTemplate)
  where
    frames = [0..frameCount-1]
    nthFrame nth = frameAt (recip (fromIntegral rate) * fromIntegral nth) ani
    frameCount = round (animationDuration ani * fromIntegral rate) :: Int
    nameTemplate :: String
    nameTemplate = "render-%05d.svg"

requireExecutable :: String -> IO FilePath
requireExecutable exec = do
  mbPath <- findExecutable exec
  case mbPath of
    Nothing   -> error $ "Couldn't find executable: " ++ exec
    Just path -> return path

runCmd exec args = do
  (ret, stdout, stderr) <- readProcessWithExitCode exec args ""
  evaluate (length stdout + length stderr)
  case ret of
    ExitSuccess -> return ()
    ExitFailure err -> do
      putStrLn $
        "Failed to run: " ++ showCommandForUser exec args ++ "\n" ++
        "Error code: " ++ show err ++ "\n" ++
        "stderr: " ++ show stderr
      throwIO (ExitFailure err)

withTempDir action = do
  dir <- getTemporaryDirectory
  (path, handle) <- openTempFile dir "reanimate-XXXXXX"
  hClose handle
  removeFile path
  createDirectory (dir </> path)
  action (dir </> path) `finally` removeDirectoryRecursive (dir </> path)

withTempFile ext action = do
  dir <- getTemporaryDirectory
  (path, handle) <- openTempFile dir ("reanimate-XXXXXX" <.> ext)
  hClose handle
  action path `finally` removeFile path
