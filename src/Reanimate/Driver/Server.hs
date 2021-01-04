{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reanimate.Driver.Server
  ( daemon
  ) where

import           Control.Concurrent
import           Control.Exception         (finally)
import qualified Control.Exception         as E
import           Control.Monad             (forM_, forever, unless, void, when)
import qualified Data.ByteString.Char8     as BS
import qualified Data.Foldable             as F
import qualified Data.Map                  as Map
import qualified Data.Text                 as T
import           Network.Socket            (AddrInfo (..), AddrInfoFlag (..), SocketOption (..),
                                            SocketType (Stream), accept, bind, close, defaultHints,
                                            getAddrInfo, gracefulClose, listen, socket,
                                            setCloseOnExecIfNeeded, setSocketOption, withFdSocket,
                                            withSocketsDo)
import           Network.Socket.ByteString (recv)
import           Network.WebSockets
import           Paths_reanimate           (getDataFileName)
import           System.IO                 (hPutStrLn, stderr)
import           Web.Browser               (openBrowser)

opts :: ConnectionOptions
opts = defaultConnectionOptions
  { connectionCompressionOptions = PermessageDeflateCompression defaultPermessageDeflate }



daemon :: IO ()
daemon = do
  state <- newMVar (0, Map.empty)
  connsRef <- newMVar Map.empty

  self <- myThreadId

  dTid <- daemonReceive self $ \msg ->
    case msg of
      WebStatus _status -> return ()
      WebError _err -> return ()
      WebFrameCount count -> do
        void $ swapMVar state (count, Map.empty)
        conns <- readMVar connsRef
        F.forM_ conns $ \(conn) -> do
          sendWebMessage conn (WebFrameCount count)
        when (Map.null conns) openViewer
      WebFrame nth path -> do
        modifyMVar_ state $ \(count, frames) ->
          pure (count, Map.insert nth path frames)
        conns <- readMVar connsRef
        F.forM_ conns $ \conn -> do
          sendWebMessage conn (WebFrame nth path)

  openViewer

  let options = ServerOptions
        { serverHost = "127.0.0.1"
        , serverPort = 9161
        , serverConnectionOptions = opts
        , serverRequirePong = Nothing }

  logMsg "WS server is running."


  runServerWithOptions options (\pending -> do
        tid <- myThreadId

        conn <- acceptRequest pending

        modifyMVar_ connsRef $ pure . Map.insert tid conn

        conns <- readMVar connsRef
        logMsg $ "Browser connections: " ++ show (Map.size conns)

        (count, frames) <- readMVar state
        when (count > 0) $ do
          sendWebMessage conn (WebFrameCount count)
          forM_ (Map.toList frames) $ \(nth, path) ->
            sendWebMessage conn (WebFrame nth path)

        let loop = do
              -- FIXME: We don't use msg here.
              _msg <- receiveData conn :: IO T.Text
              loop
            cleanup = do
              modifyMVar_ connsRef $ pure . Map.delete tid
              nConns <- Map.size <$> readMVar connsRef
              logMsg $ "Browser connections: " ++ show nConns
              when (nConns == 0) $ do
                threadDelay (second * 5)
                nConns' <- Map.size <$> readMVar connsRef
                logMsg $ "Browser connections (check): " ++ show nConns'
                when (nConns'==0) $ killThread self
        loop `finally` cleanup)
     `finally` (killThread dTid >> logMsg "daemon server quit")
     `E.catch` (\e@E.SomeException{} -> logMsg $ "Exception: " ++ show e)

second :: Int
second = 10^(6::Int)

logMsg :: String -> IO ()
logMsg msg = appendFile "log" (msg++"\n")

daemonReceive :: ThreadId -> (WebMessage -> IO ()) -> IO ThreadId
daemonReceive parent cb = withSocketsDo $ do
    addr <- resolve
    sock <- open addr
    logMsg "daemon sock is open"
    forkIO $ handler sock `finally` close sock
  where
    handler sock = forever $ E.bracketOnError (accept sock) (close . fst) $ \(conn, _peer) -> do
      inp <- BS.unpack <$> recv conn 4096
      case words inp of
        ["frame_count", n]   -> cb $ WebFrameCount (read n)
        ["frame", nth, path] -> cb $ WebFrame (read nth) path
        ["stop"]             -> do
          logMsg "Received STOP"
          killThread parent
        []                   -> return ()
        _                    -> error $ "Bad message: " ++ inp
      gracefulClose conn 5000
    resolve = do
        let hints = defaultHints {
                addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              }
        head <$> getAddrInfo (Just hints) (Just "127.0.0.1") (Just "9162")
    oSocket addr = socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    open addr = E.bracketOnError (oSocket addr) close $ \sock -> do
      setSocketOption sock ReuseAddr 1
      withFdSocket sock setCloseOnExecIfNeeded
      bind sock $ addrAddress addr
      listen sock 1024
      return sock

openViewer :: IO ()
openViewer = do
  url <- getDataFileName "viewer-elm/dist/index.html"
  bSucc <- openBrowser url
  unless bSucc $
    hPutStrLn stderr $ "Failed to open browser. Manually visit: " ++ url

-------------------------------------------------------------------------------
-- Websocket API

data WebMessage
  = WebStatus String
  | WebError String
  | WebFrameCount Int
  | WebFrame Int FilePath

sendWebMessage :: Connection -> WebMessage -> IO ()
sendWebMessage conn msg = sendTextData conn $
  case msg of
    WebStatus txt   -> T.pack "status\n" <> T.pack txt
    WebError txt    -> T.pack "error\n" <> T.pack txt
    WebFrameCount n -> T.pack $ "frame_count\n" ++ show n
    WebFrame n path -> T.pack $ "frame\n" ++ show n ++ "\n" ++ path
