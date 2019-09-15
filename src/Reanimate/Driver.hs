{-# LANGUAGE RecordWildCards #-}
module Reanimate.Driver ( reanimate ) where

import           Control.Concurrent           (MVar, forkIO, forkOS, killThread,
                                               modifyMVar_, newEmptyMVar,
                                               putMVar, takeMVar)
import           Control.Exception            (SomeException, finally, handle)
import           Control.Monad
import           Control.Monad.Fix            (fix)
import           Data.Maybe
import qualified Data.Text                    as T
import qualified Data.Text.Read               as T
import           Data.Version
import           GHC.Environment              (getFullArgs)
import           Network.WebSockets
import           Paths_reanimate
import           Reanimate.Driver.Check
import           Reanimate.Driver.CLI
import           Reanimate.Driver.Server
import           Reanimate.Misc               (runCmdLazy, runCmd_)
import           Reanimate.Monad              (Animation)
import           Reanimate.Render             (Format (..), render,
                                               renderSnippets, renderSvgs)
import           System.Directory             (doesFileExist, findExecutable,
                                               findFile, listDirectory,
                                               withCurrentDirectory)
import           System.Environment           (getArgs, getProgName)
import           System.Exit
import           System.FilePath
import           System.FSNotify
import           System.IO
import           System.IO.Temp
import           Text.ParserCombinators.ReadP
import qualified Text.PrettyPrint.ANSI.Leijen as Doc
import           Text.Printf
import           Web.Browser                  (openBrowser)

-- data Preset = Youtube | ExampleGif | Quick
presetFormat Youtube = RenderMp4
presetFormat ExampleGif = RenderGif
presetFormat Quick = RenderMp4

presetFPS Youtube = 60
presetFPS ExampleGif = 24
presetFPS Quick = 15

presetWidth Youtube = 2560
presetWidth ExampleGif = 320
presetWidth Quick = 320

presetHeight Youtube = 1440
presetHeight ExampleGif = 180
presetHeight Quick = 180

formatFPS RenderMp4 = 60
formatFPS RenderGif = 24
formatFPS RenderWebm = 60

formatWidth RenderMp4 = 2560
formatWidth RenderGif = 320
formatWidth RenderWebm = 2560

formatHeight RenderMp4 = 1440
formatHeight RenderGif = 180
formatHeight RenderWebm = 1440

reanimate :: Animation -> IO ()
reanimate animation = do
  Options{..} <- getDriverOptions
  case optsCommand of
    Raw        -> renderSvgs animation
    Test       -> renderSnippets animation
    Check      -> checkEnvironment
    View       -> serve animation
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
            RenderMp4 -> replaceExtension self "mp4"
            RenderGif -> replaceExtension self "gif"
            RenderWebm -> replaceExtension self "webm"
        Just target -> pure target

      let fps = guessParameter renderFPS (fmap presetFPS renderPreset) $
                (formatFPS fmt)
          width = guessParameter renderWidth (fmap presetWidth renderPreset) $
                  (formatWidth fmt)
          height = guessParameter renderHeight (fmap presetHeight renderPreset) $
                  (formatHeight fmt)

      print (fps, width, height, fmt, target)
      render animation target fmt width height fps

guessParameter :: Maybe a -> Maybe a -> a -> a
guessParameter a b def = fromMaybe def (a `mplus` b)
