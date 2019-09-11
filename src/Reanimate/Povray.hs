{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reanimate.Povray
  ( povray
  , povrayQuick
  , povraySlow
  ) where

import           Codec.Picture.Png
import           Control.Exception     (SomeException, handle)
import qualified Data.ByteString       as B
import qualified Data.ByteString.Lazy  as BL
import           Data.IORef
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Data.Monoid
import           Reanimate.Cache
import           Reanimate.Cache
import           Reanimate.Memo
import           Reanimate.Misc
import           Reanimate.Raster
import           Reanimate.Svg
import           System.IO

import           System.FilePath       (replaceExtension, takeFileName, (</>))
import           System.IO.Unsafe      (unsafePerformIO)

import           Control.Lens          (over, set, (%~), (&), (.~), (^.))
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as T
import           Graphics.SvgTree      (Document (..), Tree (..), defaultSvg,
                                        elements, loadSvgFile, parseSvgFile,
                                        xmlOfDocument)
import           Text.XML.Light        (elContent)
import           Text.XML.Light.Output (ppcContent, ppcElement, prettyConfigPP)

povrayRaw :: [String] -> Text -> Tree
povrayRaw args script =
  memo [Key mkPovrayImage, KeyPrim args, KeyPrim script]
  (unsafePerformIO $ mkPovrayImage args script)

povray :: [String] -> Text -> Tree
povray args = povrayRaw (["+H180","+W320", "+A"] ++ args)

povrayQuick :: [String] -> Text -> Tree
povrayQuick args = povrayRaw (["+H180","+W320"] ++ args)

povraySlow :: [String] -> Text -> Tree
povraySlow args = povrayRaw (["+H1440","+W2560", "+A"] ++ args)

mkPovrayImage :: [String] -> Text -> IO Tree
mkPovrayImage args script = cacheDiskKey key $ do
  hPutStrLn stderr "gen image"
  povray <- requireExecutable "povray"
  withTempFile "pov" $ \pov_file -> do
    let out = replaceExtension pov_file "png"
    T.writeFile pov_file script
    ret <- runCmd_ povray (args ++ ["-D","+UA", pov_file, "+o"++out])
    case ret of
      Left{} -> error "povray went wrong"
      Right{} -> do
        png <- B.readFile out
        case decodePng png of
          Left{}    -> error "bad image"
          Right img -> return $ center $ scaleToSize 320 180 $ embedDynamicImage img
  where
    key = T.concat (script:map T.pack args)
