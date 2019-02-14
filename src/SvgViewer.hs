import System.Environment (getArgs)

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo.SVG

import Control.Concurrent
import Control.Monad
import System.IO
import System.Posix.IO
import Data.IORef
import Data.Time

import Reanimate.Arrow
import Reanimate.Examples

animation = scaling

main :: IO ()
main = do

  frames <- newIORef (0::Int)
  ref <- newIORef True
  forkIO $ forever $ do
    fps <- readIORef frames
    writeIORef frames 0
    -- putStrLn $ "FPS: " ++ show fps
    threadDelay (10^6)
    modifyIORef ref not

  -- (file1:file2:_) <- getArgs
  (width, height) <- svgGetSize <$> svgNewFromString (show $ frameAt 0 animation)
  --
  -- svg2 <- svgNewFromFile file2

  t1 <- getCurrentTime

  initGUI
  dia <- dialogNew
  dialogAddButton dia stockOk ResponseOk
  contain <- dialogGetUpper dia
  canvas <- drawingAreaNew
  onSizeRequest canvas $ return (Requisition width height)
  -- (readFd, writeFd) <- createPipe
  -- readHandle <- fdToHandle readFd
  -- writeHandle <- fdToHandle writeFd
  canvas `on` exposeEvent $ do
    liftIO $ modifyIORef frames succ
    flag <- liftIO $ readIORef ref
    svg <- liftIO $ do
      t2 <- getCurrentTime
      let time = realToFrac (diffUTCTime t2 t1)
      svgNewFromString (show $ frameAt time animation)
    updateCanvas canvas svg
  flip idleAdd priorityDefaultIdle $ do
    widgetQueueDraw canvas
    return True
  boxPackStartDefaults contain canvas
  widgetShow canvas
  dialogRun dia
  return ()

updateCanvas :: DrawingArea -> SVG -> EventM EExpose Bool
updateCanvas canvas svg = do
  win <- eventWindow
  liftIO $ do
   let (width, height) = svgGetSize svg
   (width', height') <- widgetGetSize canvas
   renderWithDrawable win $ do
     scale (realToFrac width'  / realToFrac width)
           (realToFrac height' / realToFrac height)
     svgRender svg
   return True
