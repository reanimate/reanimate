{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Monoid
import Data.Time
import Network.WebSockets
import Control.Monad.Fix
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map as Map

import Cache
import Reanimate.Misc

main :: IO ()
main = do
  runServerWith "127.0.0.1" 9161 opts $ \pending -> do
  conn <- acceptRequest pending
  thread <- newEmptyMVar
  forkPingThread conn 30
  forever $ do
    msg <- receiveData conn :: IO T.Text
    stopWorker conn thread
    putMVar thread =<< forkIO (generateResponse conn msg)
  where
    opts = defaultConnectionOptions
      { connectionCompressionOptions = PermessageDeflateCompression defaultPermessageDeflate }

stopWorker conn mvar = do
  mbTid <- tryTakeMVar mvar
  case mbTid of
    Nothing -> return ()
    Just tid -> do
      putStrLn "Interrupt"
      killThread tid
      sendTextData conn $ T.pack $ "Error" ++ "Reset"

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
          ,"import           Codec.Picture.Types"
          ,"import           Reanimate.Svg"
          ,"import           Reanimate.Render"
          ,"import           Data.Monoid"
          ,"import           Graphics.Svg as S"
          ,"main = renderSvgs animation " <> T.pack (show tmpDir)
          ,"#line 1 \"animation.hs\""
          ] <> msg
        putStrLn $ "Compiling program:\n" ++ T.unpack msg
        sendTextData conn (T.pack "Compiling")
        ret <- timeIt "compile" $
                runCmd_ "stack" $ ["ghc", "--"] ++ ghcOptions ++ [tmpSource, "-o", tmpExecutable]
        case ret of
          Left err -> do
            sendTextData conn $ T.pack $ "Error" ++ unlines (drop 3 (lines err))
          Right{} -> do
            queue <- newChan
            tid <- forkIO $ forever $ sendTextData conn =<< readChan queue
            sendTextData conn (T.pack "Rendering")
            flip onException (killThread tid) $
              timeIt "render" $ withTimeout queue conn 60 $ do
              getFrame <- runCmdLazy tmpExecutable ["+RTS", "-N", "-M50M", "-RTS"]
              flip fix [] $ \loop acc -> do
                frame <- getFrame
                case frame of
                  Left "" -> do
                    writeChan queue (T.pack "Done")
                    insertCache msg (reverse acc)
                  Left err -> do
                    _ <- getChanContents queue
                    writeChan queue $ T.pack $ "Error" ++ err
                  Right frame -> do
                    writeChan queue frame
                    loop (frame : acc)

ghcOptions :: [String]
ghcOptions = ["-rtsopts", "--make", "-threaded", "-O2"]

withTimeout queue conn t action = do
  finished <- newEmptyMVar
  worker <- forkIO (action >> putMVar finished ())
  timer <- forkIO $ do
    threadDelay (10^6 * t)
    putMVar finished ()
    putStrLn "Timeout"
    killThread worker
    _ <- getChanContents queue
    writeChan queue $ T.pack $ "Error" ++ "Timeout"

  takeMVar finished `onException` do
    killThread worker
    killThread timer

timeIt :: String -> IO a -> IO a
timeIt label fn = do
  t1 <- getCurrentTime
  a <- fn
  t2 <- getCurrentTime
  putStrLn $ label ++ ": " ++ show (diffUTCTime t2 t1)
  return a
