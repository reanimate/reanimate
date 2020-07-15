{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main (main) where

import           Control.Applicative
import           Control.Concurrent     (forkIO)
import           Control.Exception
import           Control.Monad
import           Data.Bits
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS
import           Data.Char              (toLower)
import           Data.Hashable
import           Data.IORef
import           Data.Maybe
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import           Data.Time.Format
import           Discord
import qualified Discord.Requests       as R
import           Discord.Types
import           GitHash
import           Language.Haskell.Ghcid
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.Process
import           System.Timeout

gi :: GitInfo
gi = $$tGitInfoCwd

botCommitDate :: UTCTime
Just botCommitDate = parseTimeM
  True
  defaultTimeLocale
  "%a %b %d %X %Y %z" (giCommitDate gi)

botVersion :: Text
botVersion = T.pack $
  formatTime defaultTimeLocale "%F" botCommitDate

computeLimit :: Int
computeLimit = 5 * 10^(6::Int) -- 5 seconds

-- Maximum size of error messages
charLimit :: Int
charLimit = 2000

memoryLimit :: String
memoryLimit = "-M1G"

{- Commands:

>> code

:doc

:version

:clear-cache
-}

main :: IO ()
main = forever $ handle (\SomeException{} -> return ()) $ do
  tok <- getEnv "DISCORD_TOKEN"

  let p = proc "stack" ["exec", "ghci", "--rts-options="++memoryLimit]
  (ghci, _loads) <- startGhciProcess p (\_stream msg -> putStrLn msg)

  void $ exec ghci ":m + System.Environment"
  void $ exec ghci ":m + Reanimate Reanimate.Builtin.Documentation"
  void $ exec ghci ":m + Reanimate.Builtin.Images"
  void $ exec ghci ":m + Reanimate.Builtin.CirclePlot"
  void $ exec ghci ":m + Reanimate.Builtin.TernaryPlot"
  void $ exec ghci ":m + Codec.Picture.Types"
  void $ exec ghci ":set -XOverloadedStrings"

  T.putStrLn =<< runDiscord def
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
        case cmd of
          Animate script -> do
            putStrLn $ "Running script: " ++ T.unpack script
            _ <- forkIO $ void $ restCall dis (R.CreateReaction (messageChannel m, messageId m) "eyes")
            ret <- cachedRender ghci script
            case ret of
              Right vid -> do
                putStrLn "Video rendered!"
                void $ restCall dis (R.CreateMessageUploadFile (messageChannel m) "video.mp4" vid)
                void $ restCall dis (R.DeleteOwnReaction (messageChannel m, messageId m) "eyes")
                return ()
              Left err -> do
                putStrLn "Video failed!"
                Right dm <- restCall dis (R.CreateDM (userId $ messageAuthor m))
                void $ restCall dis (R.CreateMessage (channelId dm) err)
                void $ restCall dis (R.CreateReaction (messageChannel m, messageId m) "poop")
                void $ restCall dis (R.DeleteOwnReaction (messageChannel m, messageId m) "eyes")
          Version ->
            void $ restCall dis (R.CreateMessage (messageChannel m) botVersion)
          Doc _ -> return () -- not implemented
      _ -> pure ()

fromHuman :: Message -> Bool
fromHuman = not . fromBot

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

data Command
  = Animate Text
  | Version
  | Doc Text

parseCmd :: Message -> Maybe Command
parseCmd m =
    Animate <$> T.stripPrefix ">> " t <|>
    (guard (T.map toLower t==":version") >> pure Version) <|>
    Doc <$> T.stripPrefix ":doc" t
  where
    t = messageText m

renderVideo :: Ghci -> T.Text -> IO (Maybe Text)
renderVideo ghci cmd = do
  let script =
        ":{\n\
        \{-# LINE 1 \"discord\" #-}\n\
        \reanimate $ docEnv $ \
        \adjustDuration (min 10) \
        \(" ++ T.unpack cmd ++ ")\n\
        \:}"
  stderr <- newIORef []
  _ <- exec ghci ":set prog discord"
  _ <- exec ghci ":set args render --target video.mp4 --width 320 --fps 30"
  timer <- timeout computeLimit $ execStream ghci script $ \strm s ->
            when (strm == Stderr) $
              modifyIORef stderr (T.pack s:)
  if isNothing timer
    then do
      interrupt ghci
      return $ Just "timeout"
    else do
      err <- readIORef stderr
      if null err
        then return Nothing
        else return $ Just $ T.strip $ T.take charLimit $ T.unlines $ reverse err

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
          -- can't use 'renameFile' because cache might be mounted separately.
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
