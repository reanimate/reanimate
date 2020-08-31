{-|
Copyright   : Written by David Himmelstrup
License     : Unlicense
Maintainer  : lemmih@gmail.com
Stability   : experimental
Portability : POSIX
-}
module Reanimate.ColorSpace
  ( Nanometer
  , coneSensitivity
  , bigXYZCoordinates
  , lightXYZCoordinates
  , nmToColor
  ) where

import qualified Data.ByteString.Lazy       as BS
import           Data.Colour.CIE
import           Data.Csv
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Maybe
import qualified Data.Vector                as V
import           Paths_reanimate
import           System.IO.Unsafe

-- | Wavelengths in nanometers.
type Nanometer = Integer

{-# NOINLINE lightXYZCoordinates #-}
-- | (small) xyz values for each wavelength of light.
lightXYZCoordinates :: Map Nanometer (Double, Double, Double)
lightXYZCoordinates = unsafePerformIO $ do
  dat <- BS.readFile =<< getDataFileName "data/CIExyz.csv"
  case decode NoHeader dat of
    Left err -> error err
    Right vec -> return $ Map.fromList
      [ (nm, (x,y,z)) | (nm,x,y,z) <- V.toList vec, nm <= 700 ]

{-# NOINLINE bigXYZCoordinates #-}
-- | (big) XYZ values for each wavelength of light.
bigXYZCoordinates :: Map Nanometer (Double, Double, Double)
bigXYZCoordinates = unsafePerformIO $ do
  dat <- BS.readFile =<< getDataFileName "data/CIE_XYZ.csv"
  case decode NoHeader dat of
    Left err -> error err
    Right vec -> return $ Map.fromList
      [ (nm, (x,y,z)) | (nm,x,y,z) <- V.toList vec, nm <= 700 ]

-- | Helper function for converting a wavelength of light into the
--   perceived color.
nmToColor :: Nanometer -> Maybe (Colour Double)
nmToColor nm = do
  (x, y, z) <- Map.lookup nm bigXYZCoordinates
  return $ cieXYZ x y z

{-# NOINLINE coneSensitivity #-}
-- (Long, Medium, Short)
-- | Cone sensitivity by light wavelength.
coneSensitivity :: Map Nanometer (Double, Double, Double)
coneSensitivity = unsafePerformIO $ do
  dat <- BS.readFile =<< getDataFileName "data/cone_sensitivity_lms.csv"
  case decode NoHeader dat of
    Left err -> error err
    Right vec -> return $ Map.fromList
      [ (nm, (l,m,fromMaybe 0 s)) | (nm,l,m,s) <- V.toList vec, nm <= 700 ]
