{-# LANGUAGE RecordWildCards #-}
module Reanimate.Driver ( reanimate ) where

import           Control.Monad
import           Data.Maybe
import           Reanimate.Driver.Check
import           Reanimate.Driver.CLI
import           Reanimate.Driver.Compile
import           Reanimate.Driver.Server
import           Reanimate.Monad          (Animation)
import           Reanimate.Render         (FPS, Format (..), Height, Width,
                                           render, renderSnippets, renderSvgs)
import           System.FilePath
import           System.Directory
import           Text.Printf

presetFormat :: Preset -> Format
presetFormat Youtube    = RenderMp4
presetFormat ExampleGif = RenderGif
presetFormat Quick      = RenderMp4

presetFPS :: Preset -> FPS
presetFPS Youtube    = 60
presetFPS ExampleGif = 24
presetFPS Quick      = 15

presetWidth :: Preset -> Width
presetWidth Youtube    = 2560
presetWidth ExampleGif = 320
presetWidth Quick      = 320

presetHeight :: Preset -> Height
presetHeight Youtube    = 1440
presetHeight ExampleGif = 180
presetHeight Quick      = 180

formatFPS :: Format -> FPS
formatFPS RenderMp4  = 60
formatFPS RenderGif  = 24
formatFPS RenderWebm = 60

formatWidth :: Format -> Width
formatWidth RenderMp4  = 2560
formatWidth RenderGif  = 320
formatWidth RenderWebm = 2560

formatHeight :: Format -> Height
formatHeight RenderMp4  = 1440
formatHeight RenderGif  = 180
formatHeight RenderWebm = 1440

reanimate :: Animation -> IO ()
reanimate animation = do
  Options{..} <- getDriverOptions
  case optsCommand of
    Raw        -> renderSvgs animation
    Test       -> do
      hSetBinaryMode stdout True
      renderSnippets animation
    Check      -> checkEnvironment
    View       -> serve
    Render{..} -> do
      let fmt = guessParameter renderFormat (fmap presetFormat renderPreset) $
                case renderTarget of
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
          self <- findOwnSource
          pure $ case fmt of
            RenderMp4  -> replaceExtension self "mp4"
            RenderGif  -> replaceExtension self "gif"
            RenderWebm -> replaceExtension self "webm"
        Just target -> makeAbsolute target

      let fps = guessParameter renderFPS (fmap presetFPS renderPreset) $
                (formatFPS fmt)
          width = guessParameter renderWidth (fmap presetWidth renderPreset) $
                  (formatWidth fmt)
          height = guessParameter renderHeight (fmap presetHeight renderPreset) $
                  (formatHeight fmt)

      if renderCompile
        then
          compile
            ["render"
            ,"--fps", show fps
            ,"--width", show width
            ,"--height", show height
            ,"--format", showFormat fmt
            ,"--target", target
            ,"+RTS", "-N", "-RTS"]
        else do
          printf "Animation options:\n\
                 \  fps:    %d\n\
                 \  width:  %d\n\
                 \  height: %d\n\
                 \  fmt:    %s\n\
                 \  target: %s\n"
            fps width height (showFormat fmt) target
          render animation target fmt width height fps



guessParameter :: Maybe a -> Maybe a -> a -> a
guessParameter a b def = fromMaybe def (a `mplus` b)
