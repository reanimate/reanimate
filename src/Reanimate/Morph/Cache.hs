module Reanimate.Morph.Cache
  ( cachePointCorrespondence -- :: Int -> PointCorrespondence -> PointCorrespondence
  ) where

import           Control.Exception
import qualified Data.ByteString        as B
import           Data.Hashable
import           Data.Serialize
import           Reanimate.Cache        (encodeInt)
import           Reanimate.Misc         (renameOrCopyFile)
import           Reanimate.Morph.Common
import           System.Directory
import           System.FilePath
import           System.IO
import           System.IO.Temp
import           System.IO.Unsafe

-- type PointCorrespondence = Polygon → Polygon → (Polygon, Polygon)
cachePointCorrespondence :: Int -> PointCorrespondence -> PointCorrespondence
cachePointCorrespondence ident fn src dst = unsafePerformIO $ do
    root <- getXdgDirectory XdgCache "reanimate"
    createDirectoryIfMissing True root
    let path = root </> template
    hit <- doesFileExist path
    if hit
      then do
        inp <- B.readFile path
        case decode inp of
          Left{} -> do
            removeFile path
            gen path
          Right out -> return out
      else gen path
  where
    gen path = do
      correspondence <- evaluate (fn src dst)
      withSystemTempFile template $ \tmp h -> do
        hClose h
        B.writeFile tmp (encode correspondence)
        renameOrCopyFile tmp path
      return correspondence
    template = encodeInt key <.> "morph"
    key = hashWithSalt ident (src,dst)
