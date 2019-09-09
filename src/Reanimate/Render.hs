module Reanimate.Render
  ( render
  , renderSvgs
  , renderSnippets
  ) where

import           Control.Monad               (forM_)
import           Control.Parallel.Strategies
import           Control.Concurrent.QSemN
import           Control.Concurrent
import           Control.Exception
import qualified Data.ByteString.Lazy.Char8  as BS
import qualified Data.Text                   as T
import qualified Data.Text.IO                as T
import           Graphics.SvgTree            (Number (..))
import           Reanimate.Diagrams
import           Reanimate.Misc
import           Reanimate.Monad
import           System.Directory            (renameFile)
import           System.FilePath             (takeExtension, takeFileName,
                                              (</>))
import           System.IO
import           Text.Printf                 (printf)

renderSvgs :: Animation ->  IO ()
renderSvgs ani = do
    print frameCount
    lock <- newMVar ()

    concurrentForM_ (frameOrder rate frameCount) $ \nth -> do
      let -- frame = frameAt (recip (fromIntegral rate-1) * fromIntegral nth) ani
          now = (duration ani / (fromIntegral frameCount-1)) * fromIntegral nth
          frame = frameAt (if frameCount<=1 then 0 else now) ani
          svg = renderSvg Nothing Nothing frame
      evaluate (length svg)
      withMVar lock $ \_ -> do
        putStr (show nth)
        T.putStrLn $ T.concat . T.lines . T.pack $ svg
        hFlush stdout
  where
    rate = 60
    frameCount = round (duration ani * fromIntegral rate) :: Int

-- XXX: Merge with 'renderSvgs'
renderSnippets :: Animation ->  IO ()
renderSnippets ani = do
    print frameCount
    forM_ [0..frameCount-1] $ \nth -> do
      let now = (duration ani / (fromIntegral frameCount-1)) * fromIntegral nth
          frame = frameAt (if frameCount<=1 then 0 else now) ani
          svg = renderSvg Nothing Nothing frame
      putStr (show nth)
      T.putStrLn $ T.concat . T.lines . T.pack $ svg
  where
    frameCount = 50

frameOrder :: Int -> Int -> [Int]
frameOrder fps nFrames = worker [] fps
  where
    worker seen 0 = []
    worker seen nthFrame =
      filterFrameList seen nthFrame nFrames ++
      worker (nthFrame : seen) (nthFrame `div` 2)
filterFrameList seen nthFrame nFrames =
    filter (not.isSeen) $ [0, nthFrame .. nFrames-1]
  where
    isSeen x = any (\y -> x `mod` y == 0) seen

data Format = RenderMp4 | RenderGif | RenderWebm | RenderBlank

formatFPS :: Format -> Int
formatFPS RenderMp4   = 60
formatFPS RenderGif   = 25
formatFPS RenderWebm  = 30
formatFPS RenderBlank = 60

render :: Animation -> FilePath -> IO ()
render ani target =
  case takeExtension target of
    ".mp4"  -> renderFormat RenderMp4 ani target
    ".gif"  -> renderFormat RenderGif ani target
    ".webm" -> renderFormat RenderWebm ani target
    ""      -> renderFormat RenderBlank ani target
    ext     -> error $ "Unknown media format: " ++ show ext

renderFormat :: Format -> Animation -> FilePath -> IO ()
renderFormat format ani target = do
  putStrLn $ "Starting render of animation: " ++ show (round (duration ani)) ++ "s"
  ffmpeg <- requireExecutable "ffmpeg"
  generateFrames ani 2560 fps $ \template ->
    withTempFile "txt" $ \progress -> writeFile progress "" >>
    case format of
      RenderMp4 ->
        runCmd ffmpeg ["-r", show fps, "-i", template, "-y"
                      , "-c:v", "libx264", "-vf", "fps="++show fps
                      , "-progress", progress
                      , "-pix_fmt", "yuv420p", target]
      RenderGif -> withTempFile "png" $ \palette -> do
        runCmd ffmpeg ["-i", template, "-y"
                      ,"-vf", "fps="++show fps++",scale=320:-1:flags=lanczos,palettegen"
                      ,"-t", show (duration ani)
                      , palette ]
        runCmd ffmpeg ["-i", template, "-y"
                      ,"-i", palette
                      ,"-progress", progress
                      ,"-filter_complex"
                      ,"fps="++show fps++",scale=320:-1:flags=lanczos[x];[x][1:v]paletteuse"
                      ,"-t", show (duration ani)
                      , target]
      RenderWebm ->
        runCmd ffmpeg ["-r", show fps, "-i", template, "-y"
                      ,"-progress", progress
                      , "-c:v", "libvpx-vp9", "-vf", "fps="++show fps
                      , target]
      RenderBlank -> return ()
  where
    fps = formatFPS format

---------------------------------------------------------------------------------
-- Helpers

generateFrames ani width_ rate action = withTempDir $ \tmp -> do
    done <- newMVar 0
    let frameName nth = tmp </> printf nameTemplate nth
        rendered = [ renderSvg width height $ nthFrame n | n <- frames]
                    `using` parBuffer 16 rdeepseq
    concurrentForM_ frames $ \n -> do
      writeFile (frameName n) $ renderSvg width height $ nthFrame n
      modifyMVar_ done $ \nDone -> do
        putStr $ "\r" ++ show (nDone+1) ++ "/" ++ show frameCount
        hFlush stdout
        return (nDone+1)
    putStrLn "\n"
    action (tmp </> nameTemplate)
  where
    width = Just $ Num width_
    height = Just $ Num (width_*(9/16))
    frames = [0..frameCount-1]
    nthFrame nth = frameAt (recip (fromIntegral rate) * fromIntegral nth) ani
    frameCount = round (duration ani * fromIntegral rate) :: Int
    nameTemplate :: String
    nameTemplate = "render-%05d.svg"

concurrentForM_ :: [a] -> (a -> IO ()) -> IO ()
concurrentForM_ lst action = do
  n <- getNumCapabilities
  sem <- newQSemN n
  forM_ lst $ \elt -> do
    waitQSemN sem 1
    forkIO (action elt `finally` signalQSemN sem 1)
  waitQSemN sem n
