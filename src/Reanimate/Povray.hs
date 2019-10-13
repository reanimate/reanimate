{-# LANGUAGE OverloadedStrings   #-}
module Reanimate.Povray
  ( povray
  , povrayQuick
  , povraySlow
  ) where

import           Codec.Picture.Png
import qualified Data.ByteString   as B
import           Data.Text         (Text)
import qualified Data.Text         as T
import qualified Data.Text.IO      as T
import           Graphics.SvgTree  (Tree (..))
import           Reanimate.Cache
import           Reanimate.Misc
import           Reanimate.Raster
import           Reanimate.Svg.Constructors
import           System.FilePath   (replaceExtension)
import           System.IO.Unsafe  (unsafePerformIO)

povrayRaw :: [String] -> Text -> Tree
povrayRaw args script =
  -- memo [Key mkPovrayImage, KeyPrim args, KeyPrim script]
  (unsafePerformIO $ mkPovrayImage args script)

povray :: [String] -> Text -> Tree
povray args = povrayRaw (["+H180","+W320", "+A"] ++ args)

povrayQuick :: [String] -> Text -> Tree
povrayQuick args = povrayRaw (["+H180","+W320"] ++ args)

povraySlow :: [String] -> Text -> Tree
povraySlow args = povrayRaw (["+H1440","+W2560", "+A"] ++ args)

mkPovrayImage :: [String] -> Text -> IO Tree
mkPovrayImage args script = cacheDiskKey key $ do
  exec <- requireExecutable "povray"
  withTempFile "pov" $ \pov_file -> do
    let out = replaceExtension pov_file "png"
    T.writeFile pov_file script
    ret <- runCmd_ exec (args ++ ["-D","+UA", pov_file, "+o"++out])
    case ret of
      Left err -> error $ "povray went wrong:\n" ++ err
      Right{} -> do
        png <- B.readFile out
        case decodePng png of
          Left{}    -> error "bad image"
          Right img -> return $ center $ scaleToSize 16 9 $ embedDynamicImage img
  where
    key = T.concat (script:map T.pack args)
