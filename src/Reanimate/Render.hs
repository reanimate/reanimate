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

fps_webm :: Int
fps_webm = 30

fps_gif :: Int
fps_gif = 25

nameTemplate :: String
nameTemplate = "render-%05d.svg"

render :: Ani () -> FilePath -> IO ()
render ani target = do
  putStrLn $ "Starting render of animation: " ++ show (round (animationDuration ani)) ++ "s"
  let frames :: Int
      frames = round (animationDuration ani * fromIntegral fps)
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


renderGif :: Ani () -> FilePath -> IO ()
renderGif ani target = do
  putStrLn $ "Starting render of animation: " ++ show (round (animationDuration ani)) ++ "s"
  let frames :: Int
      frames = round (animationDuration ani * fromIntegral fps_gif)
  ffmpeg <- requireExecutable "ffmpeg"
  tmp <- getTemporaryDirectory
  forM_ [0..frames] $ \frame -> do
    let s = fromIntegral frame / fromIntegral fps_gif
    let fileName = printf nameTemplate frame
    renderToFile (tmp </> fileName) (frameAt s ani)
  rawSystem ffmpeg ["-i", tmp </> "render-%05d.svg"
                   ,"-vf", "fps="++show fps_gif++",scale=320:-1:flags=lanczos,palettegen"
                   ,"-t", show (animationDuration ani)
                   ,tmp </> "palette.png" ]
  rawSystem ffmpeg ["-i", tmp </> "render-%05d.svg"
                   ,"-i", tmp </> "palette.png"
                   ,"-filter_complex"
                   ,"fps="++show fps_gif++",scale=320:-1:flags=lanczos[x];[x][1:v]paletteuse"
                   ,"-t", show (animationDuration ani)
                   , target]
     `finally`
       forM_ [0..frames] (\frame -> do
         let fileName = printf nameTemplate frame
         removeFile (tmp </> fileName))
  removeFile (tmp </> "palette.png")
  return ()

renderWebm :: Ani () -> FilePath -> IO ()
renderWebm ani target = do
  putStrLn $ "Starting render of animation: " ++ show (round (animationDuration ani)) ++ "s"
  let frames :: Int
      frames = round (animationDuration ani * fromIntegral fps_webm)
  ffmpeg <- requireExecutable "ffmpeg"
  tmp <- getTemporaryDirectory
  forM_ [0..frames] $ \frame -> do
    let s = fromIntegral frame / fromIntegral fps_webm
    let fileName = printf nameTemplate frame
    renderToFile (tmp </> fileName) (frameAt s ani)
  rawSystem ffmpeg ["-r", show fps_webm, "-i", tmp </> "render-%05d.svg"
                   , "-c:v", "libvpx-vp9", "-vf", "fps="++show fps_webm
                   , target]
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
