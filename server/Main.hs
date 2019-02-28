{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Monoid
import Network.WebSockets
import Control.Monad.Fix
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map as Map

import Cache
import Reanimate.Misc

main :: IO ()
main = do
  runServer "127.0.0.1" 9161 $ \pending -> do
  conn <- acceptRequest pending
  thread <- newEmptyMVar
  forkPingThread conn 30
  forever $ do
    msg <- receiveData conn :: IO T.Text
    stopWorker thread
    putMVar thread =<< forkIO (generateResponse conn msg)


stopWorker mvar = do
  mbTid <- tryTakeMVar mvar
  case mbTid of
    Nothing -> return ()
    Just tid -> killThread tid

generateResponse conn msg = do
  mbCached <- lookupCache msg
  case mbCached of
    Just svgs -> do
      putStrLn "Returning cached svg."
      sendTextDatas conn (T.pack "Success!" : svgs)
      sendTextData conn (T.pack "Done")
    Nothing ->
      withTempFile ".exe" $ \tmpExecutable ->
      withTempFile ".hs" $ \tmpSource ->
      withTempDir $ \tmpDir -> do
        T.writeFile tmpSource $ T.unlines
          ["{-# LANGUAGE Arrows, OverloadedStrings, CPP #-}"
          ,"module Main where"
          ,"import Reanimate.Monad"
          ,"import           Reanimate.Combinators"
          ,"import           Reanimate.LaTeX"
          ,"import           Reanimate.Svg"
          ,"import           Reanimate.Render"
          ,"import           Data.Monoid"
          ,"import           Graphics.Svg as S"
          ,"main = renderSvgs animation " <> T.pack (show tmpDir)
          ,"#line 1 \"animation.hs\""
          ] <> msg
        putStrLn $ "Compiling program:\n" ++ T.unpack msg
        ret <- runCmd_ "stack" ["ghc", "--", "-rtsopts", "--make", "-threaded", "-O2", tmpSource, "-o", tmpExecutable]
        case ret of
          Left err -> do
            sendTextData conn $ T.pack $ "Error" ++ unlines (drop 3 (lines err))
          Right{} -> withTimeout conn 30 $ do
            sendTextData conn (T.pack "Rendering")
            getFrame <- runCmdLazy tmpExecutable ["+RTS", "-N", "-M50M", "-RTS"]
            flip fix [] $ \loop acc -> do
              frame <- getFrame
              case frame of
                Left "" -> do
                  insertCache msg (reverse acc)
                  sendTextData conn (T.pack "Done")
                Left err -> sendTextData conn $ T.pack $ "Error" ++ err
                Right frame -> do
                  sendTextData conn (frame)
                  loop (frame : acc)

withTimeout conn t action = do
  self <- myThreadId
  timer <- forkIO $ do
    threadDelay (10^6 * t)
    killThread self
    sendTextData conn $ T.pack $ "Error" ++ "Timeout"
  action
  killThread timer
