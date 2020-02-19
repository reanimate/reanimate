module Reanimate.Render
  ( render
  , renderSvgs
  , renderSnippets
  , Format(..)
  , Width, Height, FPS
  ) where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad       (forM_)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import           Graphics.SvgTree    (Number (..))
import           Numeric
import           Reanimate.Animation
import           Reanimate.Misc
import           System.Exit
import           System.FilePath     ((</>))
import           System.IO
import           Text.Printf         (printf)

renderSvgs :: Animation ->  IO ()
renderSvgs ani = do
    print frameCount
    lock <- newMVar ()

    handle errHandler $ concurrentForM_ (frameOrder rate frameCount) $ \nth -> do
      let -- frame = frameAt (recip (fromIntegral rate-1) * fromIntegral nth) ani
          now = (duration ani / (fromIntegral frameCount-1)) * fromIntegral nth
          frame = frameAt (if frameCount<=1 then 0 else now) ani
          svg = renderSvg Nothing Nothing frame
      _ <- evaluate (length svg)
      withMVar lock $ \_ -> do
        putStr (show nth)
        T.putStrLn $ T.concat . T.lines . T.pack $ svg
        hFlush stdout
  where
    rate = 60
    frameCount = round (duration ani * fromIntegral rate) :: Int
    errHandler (ErrorCall msg) = do
      hPutStrLn stderr msg
      exitWith (ExitFailure 1)

-- XXX: Merge with 'renderSvgs'
renderSnippets :: Animation ->  IO ()
renderSnippets ani = do
    forM_ [0..frameCount-1] $ \nth -> do
      let now = (duration ani / (fromIntegral frameCount-1)) * fromIntegral nth
          frame = frameAt now ani
          svg = renderSvg Nothing Nothing frame
      putStr (show nth)
      T.putStrLn $ T.concat . T.lines . T.pack $ svg
  where
    frameCount = 50 :: Integer

frameOrder :: Int -> Int -> [Int]
frameOrder fps nFrames = worker [] fps
  where
    worker _seen 0 = []
    worker seen nthFrame =
      filterFrameList seen nthFrame nFrames ++
      worker (nthFrame : seen) (nthFrame `div` 2)

filterFrameList :: [Int] -> Int -> Int -> [Int]
filterFrameList seen nthFrame nFrames =
    filter (not.isSeen) $ [0, nthFrame .. nFrames-1]
  where
    isSeen x = any (\y -> x `mod` y == 0) seen

data Format = RenderMp4 | RenderGif | RenderWebm
  deriving (Show)

type Width = Int
type Height = Int
type FPS = Int

render :: Animation
       -> FilePath
       -> Format
       -> Width
       -> Height
       -> FPS
       -> IO ()
render ani target format width height fps = do
  printf "Starting render of animation: %.1f\n" (duration ani)
  ffmpeg <- requireExecutable "ffmpeg"
  generateFrames ani width height fps $ \template ->
    withTempFile "txt" $ \progress -> writeFile progress "" >>
    case format of
      RenderMp4 ->
        runCmd ffmpeg ["-r", show fps, "-i", template, "-y"
                      , "-c:v", "libx264", "-vf", "fps="++show fps
                      , "-preset", "slow"
                      , "-crf", "18"
                      , "-movflags", "+faststart"
                      , "-progress", progress
                      , "-pix_fmt", "yuv420p", target]
      RenderGif -> withTempFile "png" $ \palette -> do
        runCmd ffmpeg ["-i", template, "-y"
                      ,"-vf", "fps="++show fps++",scale=320:-1:flags=lanczos,palettegen"
                      ,"-t", showFFloat Nothing (duration ani) ""
                      , palette ]
        runCmd ffmpeg ["-i", template, "-y"
                      ,"-i", palette
                      ,"-progress", progress
                      ,"-filter_complex"
                      ,"fps="++show fps++",scale=320:-1:flags=lanczos[x];[x][1:v]paletteuse"
                      ,"-t", showFFloat Nothing (duration ani) ""
                      , target]
      RenderWebm ->
        runCmd ffmpeg ["-r", show fps, "-i", template, "-y"
                      ,"-progress", progress
                      , "-c:v", "libvpx-vp9", "-vf", "fps="++show fps
                      , target]

---------------------------------------------------------------------------------
-- Helpers

generateFrames :: Animation -> Width -> Height -> FPS -> (FilePath -> IO a) -> IO a
generateFrames ani width_ height_ rate action = withTempDir $ \tmp -> do
    done <- newMVar (0::Int)
    let frameName nth = tmp </> printf nameTemplate nth
    handle h $ concurrentForM_ frames $ \n -> do
      writeFile (frameName n) $ renderSvg width height $ nthFrame n
      -- runCmd "inkscape"
      --   [ "--without-gui"
      --   , "--file=" ++ frameName n
      --   , "--export-png=" ++ replaceExtension (frameName n) "png" ]
      -- runCmd "rsvg-convert"
      --   [ frameName n
      --   , "--output", replaceExtension (frameName n) "png" ]
      modifyMVar_ done $ \nDone -> do
        putStr $ "\r" ++ show (nDone+1) ++ "/" ++ show frameCount
        hFlush stdout
        return (nDone+1)
    putStrLn "\n"
    -- action (tmp </> pngTemplate)
    action (tmp </> nameTemplate)
  where
    h UserInterrupt = do
      hPutStrLn stderr "\nCtrl-C detected. Trying to generate video with available frames. \
                       \Hit ctrl-c again to abort."
      return ()
    h other = throwIO other
    width = Just $ Num $ fromIntegral width_
    height = Just $ Num $ fromIntegral height_
    frames = [0..frameCount-1]
    nthFrame nth = frameAt (recip (fromIntegral rate) * fromIntegral nth) ani
    frameCount = round (duration ani * fromIntegral rate) :: Int
    nameTemplate :: String
    nameTemplate = "render-%05d.svg"

    -- pngTemplate :: String
    -- pngTemplate = "render-%05d.png"

concurrentForM_ :: [a] -> (a -> IO ()) -> IO ()
concurrentForM_ lst action = do
  n <- getNumCapabilities
  sem <- newQSemN n
  eVar <- newEmptyMVar
  forM_ lst $ \elt -> do
    waitQSemN sem 1
    emp <- isEmptyMVar eVar
    if emp
      then void $ forkIO (catch (action elt) (void . tryPutMVar eVar) `finally` signalQSemN sem 1)
      else signalQSemN sem 1
  waitQSemN sem n
  mbE <- tryTakeMVar eVar
  case mbE of
    Nothing -> return ()
    Just e  -> throwIO (e :: SomeException)
  where
    void x = x >> return ()
