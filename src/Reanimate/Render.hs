module Reanimate.Render where

import Lucid.Svg
import Control.Monad
import Control.Exception
import System.FilePath
import System.Directory
import System.Process
import Text.Printf
import Reanimate.Arrow
import Reanimate.Examples

fps :: Int
fps = 60

nameTemplate :: String
nameTemplate = "render-%05d.svg"

render :: Ani () -> FilePath -> IO ()
render ani target = do
  putStrLn $ "Starting render of animation: " ++ show (round (getDuration ani)) ++ "s"
  let frames :: Int
      frames = round (getDuration ani * fromIntegral fps)
  ffmpeg <- requireExecutable "ffmpeg"
  tmp <- getTemporaryDirectory
  forM_ [0..frames] $ \frame -> do
    let s = fromIntegral frame / fromIntegral fps
    let fileName = printf nameTemplate frame
    renderToFile (tmp </> fileName) (frameAt s ani)
  rawSystem ffmpeg ["-r", show fps, "-i", tmp </> "render-%05d.svg"
                       , "-c:v", "libx264", "-vf", "fps="++show fps
                       , "-pix_fmt", "yuv420p", target]
     `finally`
       forM_ [0..frames] (\frame -> do
         let fileName = printf nameTemplate frame
         removeFile (tmp </> fileName))
  return ()


requireExecutable :: String -> IO FilePath
requireExecutable exec = do
  mbPath <- findExecutable exec
  case mbPath of
    Nothing -> error $ "Couldn't find executable: " ++ exec
    Just path -> return path
