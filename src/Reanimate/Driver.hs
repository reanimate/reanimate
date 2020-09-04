{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE RecordWildCards #-}
module Reanimate.Driver
  ( reanimate
  )
where

import           Control.Applicative      ((<|>))
import           Control.Monad
import           Data.Maybe
import           Data.Either
import           Reanimate.Animation      (Animation)
import           Reanimate.Driver.Check
import           Reanimate.Driver.CLI
import           Reanimate.Driver.Compile
import           Reanimate.Driver.Server
import           Reanimate.Parameters
import           Reanimate.Render         (render, renderSnippets, renderSvgs,
                                           selectRaster)
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.IO
import           Text.Printf

presetFormat :: Preset -> Format
presetFormat Youtube    = RenderMp4
presetFormat ExampleGif = RenderGif
presetFormat Quick      = RenderMp4
presetFormat MediumQ    = RenderMp4
presetFormat HighQ      = RenderMp4
presetFormat LowFPS     = RenderMp4

presetFPS :: Preset -> FPS
presetFPS Youtube    = 60
presetFPS ExampleGif = 25
presetFPS Quick      = 15
presetFPS MediumQ    = 30
presetFPS HighQ      = 30
presetFPS LowFPS     = 10

presetWidth :: Preset -> Width
presetWidth Youtube    = 2560
presetWidth ExampleGif = 320
presetWidth Quick      = 320
presetWidth MediumQ    = 800
presetWidth HighQ      = 1920
presetWidth LowFPS     = presetWidth HighQ

presetHeight :: Preset -> Height
presetHeight preset = presetWidth preset * 9 `div` 16

formatFPS :: Format -> FPS
formatFPS RenderMp4  = 60
formatFPS RenderGif  = 25
formatFPS RenderWebm = 60

formatWidth :: Format -> Width
formatWidth RenderMp4  = 2560
formatWidth RenderGif  = 320
formatWidth RenderWebm = 2560

formatHeight :: Format -> Height
formatHeight RenderMp4  = 1440
formatHeight RenderGif  = 180
formatHeight RenderWebm = 1440

formatExtension :: Format -> String
formatExtension RenderMp4  = "mp4"
formatExtension RenderGif  = "gif"
formatExtension RenderWebm = "webm"

{-|
Main entry-point for accessing an animation. Creates a program that takes the
following command-line arguments:

> Usage: PROG [COMMAND]
>   This program contains an animation which can either be viewed in a web-browser
>   or rendered to disk.
>
> Available options:
>   -h,--help                Show this help text
>
> Available commands:
>   check                    Run a system's diagnostic and report any missing
>                            external dependencies.
>   view                     Play animation in browser window.
>   render                   Render animation to file.

Neither the \'check\' nor the \'view\' command take any additional arguments.
Rendering animation can be controlled with these arguments:

> Usage: PROG render [-o|--target FILE] [--fps FPS] [-w|--width PIXELS]
>                    [-h|--height PIXELS] [--compile] [--format FMT]
>                    [--preset TYPE]
>   Render animation to file.
>
> Available options:
>   -o,--target FILE         Write output to FILE
>   --fps FPS                Set frames per second.
>   -w,--width PIXELS        Set video width.
>   -h,--height PIXELS       Set video height.
>   --compile                Compile source code before rendering.
>   --format FMT             Video format: mp4, gif, webm
>   --preset TYPE            Parameter presets: youtube, gif, quick
>   -h,--help                Show this help text
-}
reanimate :: Animation -> IO ()
reanimate animation = do
  Options {..} <- getDriverOptions
  case optsCommand of
    Raw {..} -> do
      setFPS 60
      renderSvgs rawOutputFolder rawFrameOffset rawPrettyPrint animation
    Test -> do
      setNoExternals True
      -- hSetBinaryMode stdout True
      renderSnippets animation
    Check       -> checkEnvironment
    View {..}   -> serve viewVerbose viewGHCPath viewGHCOpts viewOrigin
    Render {..} -> do
      let fmt =
            guessParameter renderFormat (fmap presetFormat renderPreset)
              $ case renderTarget of
                  -- Format guessed from output
                  Just target -> case takeExtension target of
                    ".mp4"  -> RenderMp4
                    ".gif"  -> RenderGif
                    ".webm" -> RenderWebm
                    _       -> RenderMp4
                  -- Default to mp4 rendering.
                  Nothing -> RenderMp4

      target <- case renderTarget of
        Nothing -> do
          mbSelf <- findOwnSource
          let ext = formatExtension fmt
              self = fromMaybe "output" mbSelf
          pure $ replaceExtension self ext
        Just target -> makeAbsolute target

      let
        fps =
          guessParameter renderFPS (fmap presetFPS renderPreset) $ formatFPS fmt
        (width, height) = fromMaybe
          ( maybe (formatWidth fmt)  presetWidth  renderPreset
          , maybe (formatHeight fmt) presetHeight renderPreset
          )
          (userPreferredDimensions renderWidth renderHeight)

      raster <-
        if renderRaster == RasterNone || renderRaster == RasterAuto  then do
          svgSupport <- hasFFmpegRSvg
          if isRight svgSupport
            then selectRaster renderRaster
            else do
              raster <- selectRaster RasterAuto
              when (raster == RasterNone) $ do
                hPutStrLn stderr
                  "Error: your FFmpeg was built without SVG support and no raster engines \
                  \are available. Please install either inkscape, imagemagick, or rsvg."
                exitWith (ExitFailure 1)
              return raster
        else selectRaster renderRaster

      if renderCompile
        then compile $
          [ "render"
          , "--fps"
          , show fps
          , "--width"
          , show width
          , "--height"
          , show height
          , "--format"
          , showFormat fmt
          , "--raster"
          , showRaster raster
          , "--target"
          , target
          , "+RTS"
          , "-N"
          , "-RTS"
          ] ++ [ "--partial" | renderPartial ]
        else do
          setRaster raster
          setFPS fps
          setWidth width
          setHeight height
          printf
            "Animation options:\n\
                 \  fps:    %d\n\
                 \  width:  %d\n\
                 \  height: %d\n\
                 \  fmt:    %s\n\
                 \  target: %s\n\
                 \  raster: %s\n"
            fps
            width
            height
            (showFormat fmt)
            target
            (show raster)

          render animation target raster fmt width height fps renderPartial

guessParameter :: Maybe a -> Maybe a -> a -> a
guessParameter a b def = fromMaybe def (a <|> b)


-- If user specifies exactly one dimension explicitly, calculate the other
userPreferredDimensions :: Maybe Width -> Maybe Height -> Maybe (Width, Height)
userPreferredDimensions (Just width) (Just height) = Just (width, height)
userPreferredDimensions (Just width) Nothing =
  Just (width, makeEven $ width * 9 `div` 16)
userPreferredDimensions Nothing (Just height) =
  Just (makeEven $ height * 16 `div` 9, height)
userPreferredDimensions Nothing Nothing = Nothing

-- Avoid ffmpeg failures "height not divisible by 2"
makeEven :: Int -> Int
makeEven x | even x    = x
           | otherwise = x - 1
