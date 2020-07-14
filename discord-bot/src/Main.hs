{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Concurrent     (forkIO, threadDelay)
import           Control.Exception
import           Control.Monad          (forM_, void, when)
import           Data.Bits
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS
import           Data.Char              (toLower, isSpace)
import           Data.Hashable
import           Data.IORef
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import           Discord
import qualified Discord.Requests       as R
import           Discord.Types
import           Language.Haskell.Ghcid
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.Process

main :: IO ()
main = do
  tok <- getEnv "DISCORD_TOKEN"

  (ghci, loads) <- startGhci "stack exec ghci" Nothing (\_stream msg -> putStrLn msg)

  exec ghci ":m + System.Environment"
  exec ghci ":m + Reanimate Reanimate.Builtin.Documentation"
  exec ghci ":m + Codec.Picture.Types"

  void $ runDiscord $ def
    { discordToken = T.pack tok
    , discordOnStart = startHandler
    , discordOnEvent = eventHandler ghci
    , discordOnLog = \s -> T.putStrLn s >> T.putStrLn ""
    , discordForkThreadForEvents = False
    }

startHandler :: DiscordHandle -> IO ()
startHandler _dis = putStrLn "Ready!"

-- If an event handler throws an exception, discord-haskell will continue to run
eventHandler :: Ghci -> DiscordHandle -> Event -> IO ()
eventHandler ghci dis event = case event of
      MessageCreate m | Just cmd <- parseCmd m, fromHuman m -> do
        putStrLn $ "Running script: " ++ T.unpack cmd
        _ <- forkIO $ void $ restCall dis (R.CreateReaction (messageChannel m, messageId m) "eyes")
        ret <- cachedRender ghci cmd
        case ret of
          Right vid -> do
            putStrLn "Video rendered!"
            void $ restCall dis (R.CreateMessageUploadFile (messageChannel m) "video.mp4" vid)
            void $ restCall dis (R.DeleteOwnReaction (messageChannel m, messageId m) "eyes")
            return ()
          Left err -> do
            putStrLn "Video failed!"
            Right dm <- restCall dis (R.CreateDM (userId $ messageAuthor m))
            void $ restCall dis (R.CreateMessage (channelId dm) ("Error:\n" <> err))
            void $ restCall dis (R.DeleteOwnReaction (messageChannel m, messageId m) "eyes")
            void $ restCall dis (R.CreateReaction (messageChannel m, messageId m) "poop")
            return ()
      _ -> pure ()

fromHuman :: Message -> Bool
fromHuman = not . fromBot

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

parseCmd :: Message -> Maybe Text
parseCmd = T.stripPrefix ">> " . messageText

{- Goals
 - DONE: Error messages (in private)
 - Limit memory
 - Limit CPU time
 - Enable caching (even between upgrades)
 - DONE: Cache videos
 - Automatic upgrade and deploy
 - Command: version
 - Command: clear-cache
-}

renderVideo :: Ghci -> T.Text -> IO (Maybe Text)
renderVideo ghci cmd = do
  let script =
        "{-# LINE 2 \"discord\" #-} \
        \reanimate $ docEnv $ \
        \adjustDuration (min 10) $ \
        \(" ++ T.unpack cmd ++ ")"
  stderr <- newIORef []
  _ <- exec ghci ":set prog discord"
  _ <- exec ghci ":set args render --target video.mp4 --width 320 --fps 30"
  execStream ghci script $ \strm s ->
    when (strm == Stderr) $
      modifyIORef stderr (T.pack s:)
  err <- readIORef stderr
  if null err
    then return Nothing
    else return $ Just $ T.strip $ T.unlines $ reverse err

cachedRender :: Ghci -> Text -> IO (Either Text ByteString)
cachedRender ghci cmd = do
  root <- getXdgDirectory XdgCache "reanimate-bot"
  createDirectoryIfMissing True root
  let path = root </> encodeInt (hash cmd) <.> "mp4"
  hit <- doesFileExist path
  if hit
    then do
      putStrLn "Cache hit"
      vid <- BS.readFile path
      return $ Right vid
    else do
      putStrLn "Cache miss"
      ret <- renderVideo ghci cmd
      case ret of
        Just err -> return $ Left err
        Nothing -> do
          copyFile "video.mp4" path
          removeFile "video.mp4"
          vid <- BS.readFile path
          return $ Right vid

encodeInt :: Int -> String
encodeInt i = worker (fromIntegral i) 60
  where
    worker :: Word -> Int -> String
    worker key sh
      | sh < 0 = []
      | otherwise =
        case (key `shiftR` sh) `mod` 64 of
          idx -> alphabet !! fromIntegral idx : worker key (sh-6)
    alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+$"
