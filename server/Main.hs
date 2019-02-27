module Main where

import Control.Concurrent.STM
import Control.Concurrent
import Control.Exception
import Control.Monad
import System.Clock
import Network.WebSockets
import Control.Monad.Fix
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Cache as C
import qualified Data.Map as Map

import Reanimate.Misc

main :: IO ()
main = do
  c <- C.newCache (Just day)
  forkIO $ forever $ do
    C.purgeExpired c
    -- mergeValues c
    threadDelay (10^6 * 60)
  runServer "127.0.0.1" 9160 $ \pending -> do
  conn <- acceptRequest pending
  thread <- newEmptyMVar
  forkPingThread conn 30
  forever $ do
    msg <- receiveData conn :: IO T.Text
    stopWorker thread
    putMVar thread =<< forkIO (generateResponse c conn msg)


stopWorker mvar = do
  mbTid <- tryTakeMVar mvar
  case mbTid of
    Nothing -> return ()
    Just tid -> killThread tid

generateResponse c conn msg = do
  mbCached <- C.lookup c msg
  case mbCached of
    Just svgs -> do
      putStrLn "Returning cached svg."
      sendTextDatas conn (T.pack "Success!" : svgs)
      sendTextData conn (T.pack "Done")
    Nothing ->
      withTempFile ".exe" $ \tmpExecutable ->
      withTempFile ".hs" $ \tmpSource ->
      withTempDir $ \tmpDir -> do
        writeFile tmpSource $ unlines
          ["{-# LANGUAGE Arrows, OverloadedStrings #-}"
          ,"module Main where"
          ,"import Reanimate.Arrow"
          ,"import           Reanimate.Combinators"
          ,"import           Reanimate.LaTeX"
          ,"import           Reanimate.Svg"
          ,"import           Reanimate.Render"
          ,"import           Control.Arrow         (returnA, (>>>))"
          ,"import           Data.Monoid"
          ,"import           Lucid.Svg (toHtml)"
          ,"import           Graphics.Svg as S"
          ,"import           Lucid.Svg             (Svg, circle_, clip_path_, cx_, cy_, d_, id_, defs_, clipPath_,"
          ,"                                        fill_, fill_opacity_, font_size_, g_,"
          ,"                                        height_, line_, opacity_, path_, r_,"
          ,"                                        rect_, stroke_, stroke_width_, text_,"
          ,"                                        text_anchor_, toHtml, transform_,"
          ,"                                        width_, x1_, x2_, x_, y1_, y2_, y_)"
          ,"import qualified Lucid.Svg             as Lucid"
          ,"main = renderSvgs animation " ++ show tmpDir
          ] ++ T.unpack msg
        putStrLn $ "Compiling program:\n" ++ T.unpack msg
        ret <- runCmd_ "stack" ["ghc", "--", "-rtsopts", "--make", "-threaded", "-O2", tmpSource, "-o", tmpExecutable]
        case ret of
          Left err -> do
            sendTextData conn $ T.pack $ "Error" ++ unlines (drop 3 (lines err))
          Right{} -> withTimeout conn 10 $ do
            sendTextData conn (T.pack "Rendering")
            getFrame <- runCmdLazy tmpExecutable ["+RTS", "-N", "-M50M", "-RTS"]
            flip fix [] $ \loop acc -> do
              frame <- getFrame
              case frame of
                Left "" -> do
                  C.insert c msg (reverse acc)
                  sendTextData conn (T.pack "Done")
                Left err -> sendTextData conn $ T.pack $ "Error" ++ err
                Right frame -> do
                  sendTextData conn (frame)
                  loop (frame : acc)

day :: TimeSpec
day = TimeSpec (60*60*24) 0

withTimeout conn t action = do
  self <- myThreadId
  timer <- forkIO $ do
    threadDelay (10^6 * t)
    killThread self
    sendTextData conn $ T.pack $ "Error" ++ "Timeout"
  action `finally` killThread timer

-- mergeValues :: C.Cache String [T.Text] -> IO ()
-- mergeValues cache = do
--   keys <- C.keys cache
--   worker Map.empty keys
--   where
--     worker m [] = return ()
--     worker m (k:ks) = do
--       mbVal <- C.lookup cache k
--       case mbVal of
--         Nothing -> worker m ks
--         Just val ->
--           case Map.lookup val m of
--             Just val' -> do
--               C.insert cache k val'
--               worker m ks
--             Nothing ->
--               worker (Map.insert val val m) ks
