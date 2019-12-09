{-# LANGUAGE OverloadedStrings #-}
module Reanimate.Povray
  ( povray
  , povrayQuick
  , povraySlow
  , povrayExtreme
  , povray'
  , povrayQuick'
  , povraySlow'
  , povrayExtreme'
  ) where

import           Codec.Picture.Png
import qualified Data.ByteString            as B
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import           Graphics.SvgTree           (Tree (..))
import           Reanimate.Cache
import           Reanimate.Misc
import           Reanimate.Raster
import           Reanimate.Parameters
import           Reanimate.Svg.Constructors
import           System.FilePath            (replaceExtension, (<.>))
import           System.IO.Unsafe           (unsafePerformIO)
import           Data.Hashable

povrayRaw :: [String] -> Text -> Tree
povrayRaw args script =
  (unsafePerformIO $ mkPovrayImage args script)

povrayRaw' :: [String] -> Text -> FilePath
povrayRaw' args script =
  (unsafePerformIO $ mkPovrayImage' args script)

povray :: [String] -> Text -> Tree
povray args = povrayRaw (["+H180","+W320", "+A"] ++ args)

povray' :: [String] -> Text -> FilePath
povray' args = povrayRaw' (["+H180","+W320", "+A"] ++ args)

povrayQuick :: [String] -> Text -> Tree
povrayQuick args = povrayRaw (["+H180","+W320"] ++ args)

povrayQuick' :: [String] -> Text -> FilePath
povrayQuick' args = povrayRaw' (["+H180","+W320"] ++ args)

povraySlow :: [String] -> Text -> Tree
povraySlow args = povrayRaw (["+H1440","+W2560", "+A"] ++ args)

povraySlow' :: [String] -> Text -> FilePath
povraySlow' args = povrayRaw' (["+H1440","+W2560", "+A"] ++ args)

povrayExtreme :: [String] -> Text -> Tree
povrayExtreme args = povrayRaw (["+H2160","+W3840", "+A"] ++ args)

povrayExtreme' :: [String] -> Text -> FilePath
povrayExtreme' args = povrayRaw' (["+H2160","+W3840", "+A"] ++ args)

mkPovrayImage :: [String] -> Text -> IO Tree
mkPovrayImage _ script | pNoExternals = pure $ mkText script
mkPovrayImage args script = do
    out <- mkPovrayImage' args script
    -- return $ center $ scaleToSize 16 9 $ embedImageFile out
    png <- B.readFile out
    case decodePng png of
      Left{}    -> error "bad image"
      Right img -> return $ center $ scaleToSize 16 9 $ embedDynamicImage img

mkPovrayImage' :: [String] -> Text -> IO FilePath
mkPovrayImage' _ _ | pNoExternals = pure "/povray/has/been/disabled"
mkPovrayImage' args script = cacheFile template $ \target -> do
    exec <- requireExecutable "povray"
    let pov_file = replaceExtension target "pov"
    T.writeFile pov_file script
    runCmd exec (args ++ ["-D","+UA", pov_file, "+o"++target])
  where
    template = show (hash key) <.> "png"
    key = T.concat (script:map T.pack args)

