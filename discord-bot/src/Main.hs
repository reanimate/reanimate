{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main (main) where

import           Control.Applicative
import           Control.Concurrent           (forkIO)
import           Control.Exception
import           Control.Monad
import           Data.Bits
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as BS
import           Data.Char                    (toLower)
import           Data.Hashable
import           Data.IORef
import           Data.Maybe
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as T
import           Data.Time.Format
import           Discord
import qualified Discord.Requests             as R
import           Discord.Types
import           GitHash
import           Language.Haskell.Exts.Parser
import           Language.Haskell.Exts.Syntax (Exp)
import           Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import           Language.Haskell.Ghcid
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO
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
  formatTime defaultTimeLocale "%F" botCommitDate ++
  " (" ++ take 5 (giHash gi) ++ ")"

computeLimit :: Int
computeLimit = 15 * 10^(6::Int) -- 15 seconds

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
main = forever $ do
  -- Load library as compiled code. Documentation will not be available.
  let fastProc = proc "stack" ["exec", "ghci", "--rts-options="++memoryLimit]
  (fastGhci, _loads) <- startGhciProcess fastProc (\_stream msg -> putStrLn msg)

  void $ exec fastGhci ":m + System.Environment"
  void $ exec fastGhci ":m + Reanimate Reanimate.Builtin.Documentation"
  void $ exec fastGhci ":m + Reanimate.Builtin.Images"
  void $ exec fastGhci ":m + Reanimate.Builtin.CirclePlot"
  void $ exec fastGhci ":m + Reanimate.Builtin.TernaryPlot"
  void $ exec fastGhci ":m + Reanimate.Morph.Common"
  void $ exec fastGhci ":m + Reanimate.Morph.Linear"
  void $ exec fastGhci ":m + Reanimate.Scene"
  void $ exec fastGhci ":m + Control.Lens"
  void $ exec fastGhci ":m + Codec.Picture.Types"
  void $ exec fastGhci ":set -XOverloadedStrings"

  -- Load library as bytecode. Haddock documentation will be available.
  let slowProc = proc "stack" ["ghci", "--ghci-options=-haddock"]
  (slowGhci, _loads) <- startGhciProcess slowProc (\_stream msg -> putStrLn msg)

  void $ exec slowGhci ":set -XOverloadedStrings"

  args <- getArgs
  case args of
    ["test"] -> do
      let assertIO action checker msg = do
            val <- action
            unless (checker val) $ do
              hPutStrLn stderr msg
              exitWith (ExitFailure 1)
      assertIO (pure botVersion) (not . T.null)
        "Missing git version"
      assertIO (exec fastGhci "1+2::Int") (==["3"])
        "Eval failed: 1+2"
      assertIO (exec fastGhci ":t reanimate") (==["reanimate :: Animation -> IO ()"])
        "Can't access reanimate, fast"
      assertIO (exec slowGhci ":t reanimate") (==["reanimate :: Animation -> IO ()"])
        "Can't access reanimate, docs"
      assertIO (exec slowGhci ":doc reanimate") (/=["<has no documentation>"])
        "Can't access docs"
      assertIO (findExecutable "latex") (/=Nothing)
        "latex is missing"
      assertIO (findExecutable "xelatex") (/=Nothing)
        "xelatex is missing"
      exitWith ExitSuccess
    [] -> do
      tok <- getEnv "DISCORD_TOKEN"
      T.putStrLn =<< runDiscord def
        { discordToken = T.pack tok
        , discordOnStart = startHandler
        , discordOnEvent = eventHandler fastGhci slowGhci
        , discordOnLog = \s -> T.putStrLn s >> T.putStrLn ""
        , discordForkThreadForEvents = False
        }
    _ -> do
      hPutStrLn stderr "Invalid arguments"
      exitWith (ExitFailure 1)

startHandler :: DiscordHandle -> IO ()
startHandler _dis = putStrLn "Ready!"

-- If an event handler throws an exception, discord-haskell will continue to run
eventHandler :: Ghci -> Ghci -> DiscordHandle -> Event -> IO ()
eventHandler fastGhci slowGhci dis event = case event of
  MessageCreate m | Just cmd <- parseCmd m, fromHuman m -> do
    case cmd of
      Animate script | not (isValidHaskell script) -> do
        putStrLn "Video failed!"
        void $ restCall dis (R.CreateReaction (messageChannel m, messageId m) "poop")
        void $ restCall dis (R.DeleteOwnReaction (messageChannel m, messageId m) "eyes")
        Right dm <- restCall dis (R.CreateDM (userId $ messageAuthor m))
        void $ restCall dis (R.CreateMessage (channelId dm) "Invalid Haskell expression")
      Animate script -> do
        putStrLn $ "Running script: " ++ T.unpack script
        _ <- forkIO $ void $ restCall dis (R.CreateReaction (messageChannel m, messageId m) "eyes")
        ret <- cachedRender fastGhci script
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
      Doc expr -> do
        doc <- askDoc slowGhci expr
        void $ restCall dis (R.CreateMessage (messageChannel m) doc)
      Type expr -> do
        ty <- askType slowGhci expr
        void $ restCall dis (R.CreateMessage (messageChannel m) ty)
      Restart -> do
        void $ restCall dis (R.CreateMessage (messageChannel m) "Restarting...")
        stopDiscord dis
  _ -> pure ()

fromHuman :: Message -> Bool
fromHuman = not . fromBot

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

data Command
  = Animate Text
  | Version
  | Doc Text
  | Type Text
  | Restart

parseCmd :: Message -> Maybe Command
parseCmd m =
    Animate <$> T.stripPrefix ">> " t <|>
    (guard (T.map toLower t==":version") >> pure Version) <|>
    Doc . oneLine <$> T.stripPrefix ":doc " t <|>
    Type . oneLine <$> T.stripPrefix ":t " t <|>
    (guard (T.map toLower t==":restart") >> pure Restart)
  where
    t = messageText m
    oneLine = T.unwords . T.lines

renderVideo :: Ghci -> T.Text -> IO (Maybe Text)
renderVideo ghci cmd = do
  let script =
        ":{\n\
        \{-# LINE 1 \"discord\" #-}\n\
        \reanimate $ docEnv $ \
        \adjustDuration (min 10) \
        \(" ++ T.unpack cmd ++ "\n)\n\
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

askType :: Ghci -> Text -> IO Text
askType ghci expr =
  T.strip . T.unlines . map T.pack <$> exec ghci (":type " ++ T.unpack expr)

askDoc :: Ghci -> Text -> IO Text
askDoc ghci expr =
  cleanDocs . map T.pack <$> exec ghci (":doc " ++ T.unpack expr)

cleanDocs :: [Text] -> Text
cleanDocs = T.strip . T.unlines . worker . map T.strip
  where
    worker (x:xs) | T.null x = worker xs
    worker (x:y:xs)
      | T.null y || ">" `T.isPrefixOf` y = x : worker xs
      | otherwise = worker (x <> " " <> y : xs)
    worker (x:xs) = x : worker xs
    worker [] = []

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

isValidHaskell :: Text -> Bool
isValidHaskell txt =
  case parse (T.unpack txt) :: ParseResult (Exp SrcSpanInfo) of
    ParseOk{}     -> True
    ParseFailed{} -> False
