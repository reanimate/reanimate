module Reanimate.Driver.Magick
  ( magickCmd
  ) where

import System.IO.Unsafe (unsafePerformIO)
import System.Directory (findExecutable)

{-# NOINLINE magickCmd #-}
-- |The name of the ImageMagick command. On Unix-like operating systems, the
-- command \'convert\' does not conflict with the name of other commands. On
-- Windows, ImageMagick version 7 is readily available, the command \'magick\'
-- should be present, and is preferred over \'convert\'. If it is not present,
-- \'convert\' is assumed to be the relevant command.
magickCmd :: String
-- The use of 'unsafeperformIO' is justified on the basis that if \'magick\' is
-- found once, it will always be present.
magickCmd = unsafePerformIO $ do
  mPath <- findExecutable "magick"
  pure $ maybe "convert" (const "magick") mPath
