module Reanimate.Render
  ( render
  , renderSvgs
  ) where

import           Control.Monad               (forM_)
import           Control.Parallel.Strategies
import qualified Data.ByteString.Lazy.Char8  as BS
import qualified Data.Text                   as T
import qualified Data.Text.IO                as T
import           Graphics.SvgTree            (Number (..))
import           Reanimate.Diagrams
import           Reanimate.Examples
import           Reanimate.Misc
import           Reanimate.Monad
import           System.Directory            (renameFile)
import           System.FilePath             (takeExtension, takeFileName,
                                              (</>))
import           Text.Printf                 (printf)

renderSvgs :: Animation -> FilePath -> IO ()
renderSvgs ani tmpDir = do
    let frameName nth = tmpDir </> printf nameTemplate nth
        renderedFrames = map (T.concat . T.lines . T.pack . nthFrame) frames
    mapM_ T.putStrLn (renderedFrames `using` parBuffer 16 rdeepseq)
  where
    frames = [0..frameCount-1]
    rate = 60
    nthFrame nth = renderTree $ frameAt (recip (fromIntegral rate) * fromIntegral nth) ani
    frameCount = round (duration ani * fromIntegral rate) :: Int
    nameTemplate :: String
    nameTemplate = "render-%05d.svg"


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
  generateFrames ani 320 fps $ \template ->
    case format of
      RenderMp4 ->
        runCmd ffmpeg ["-r", show fps, "-i", template, "-y"
                      , "-c:v", "libx264", "-vf", "fps="++show fps
                      , "-pix_fmt", "yuv420p", target]
      RenderGif -> withTempFile "png" $ \palette -> do
        runCmd ffmpeg ["-i", template, "-y"
                      ,"-vf", "fps="++show fps++",scale=320:-1:flags=lanczos,palettegen"
                      ,"-t", show (duration ani)
                      , palette ]
        runCmd ffmpeg ["-i", template, "-y"
                      ,"-i", palette
                      ,"-filter_complex"
                      ,"fps="++show fps++",scale=320:-1:flags=lanczos[x];[x][1:v]paletteuse"
                      ,"-t", show (duration ani)
                      , target]
      RenderWebm ->
        runCmd ffmpeg ["-r", show fps, "-i", template, "-y"
                      , "-c:v", "libvpx-vp9", "-vf", "fps="++show fps
                      , target]
      RenderBlank -> return ()
  where
    fps = formatFPS format

---------------------------------------------------------------------------------
-- Helpers
-- XXX: Move to a different module and unify with helpers from LaTeX.

-- XXX: Use threads
generateFrames ani width_ rate action = withTempDir $ \tmp -> do
    let frameName nth = tmp </> printf nameTemplate nth
        rendered = [ renderSizedTree width height $ nthFrame n | n <- frames]
                    `using` parBuffer 16 rdeepseq
    forM_ (zip [0::Int ..] rendered) $ \(n, frame) -> do
      writeFile (frameName n) frame
      putStr $ "\r" ++ show (n+1) ++ "/" ++ show frameCount
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
