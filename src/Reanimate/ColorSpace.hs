module Reanimate.ColorSpace where

import qualified Data.ByteString.Lazy as BS
import           Data.Csv
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.Maybe
import qualified Data.Vector          as V
import Data.Colour.CIE
import Data.Colour.CIE.Illuminant
import           Paths_reanimate
import           System.IO.Unsafe

import           Reanimate.Svg
import           Graphics.SvgTree (Number(..), Tree)

type Nanometer = Integer


lightXYZCoordinates :: Map Nanometer (Double, Double, Double)
lightXYZCoordinates = unsafePerformIO $ do
  dat <- BS.readFile =<< getDataFileName "data/CIExyz.csv"
  case decode NoHeader dat of
    Left err -> error err
    Right vec -> return $ Map.fromList
      [ (nm, (x,y,z)) | (nm,x,y,z) <- V.toList vec, nm <= 700 ]

renderXYZCoordinates :: Tree
renderXYZCoordinates =
  withFillOpacity 0 $
  mkLinePath $
  [ (x, 1-y)
  | (ang, (x,y,_z)) <- Map.toList lightXYZCoordinates
  ]

lightLABCoordinates :: Map Nanometer (Double, Double, Double)
lightLABCoordinates = Map.map fn lightXYZCoordinates
  where
    fn (x,y,z) = cieLABView d65 (cieXYZ x y z)

renderLABCoordinates :: Tree
renderLABCoordinates =
  withFillOpacity 0 $
  mkLinePath $
  [ (a/350, (1-b)/150)
  | (ang, (_l,a,b)) <- Map.toList lightLABCoordinates
  ]

-- (Long, Medium, Short)
coneSensitivity :: Map Nanometer (Double, Double, Double)
coneSensitivity = unsafePerformIO $ do
  dat <- BS.readFile =<< getDataFileName "data/cone_sensitivity_lms.csv"
  case decode NoHeader dat of
    Left err -> error err
    Right vec -> return $ Map.fromList
      [ (nm, (l,m,fromMaybe 0 s)) | (nm,l,m,s) <- V.toList vec ]

renderSensitivity :: Tree
renderSensitivity = mkGroup
  [ withStrokeColor "blue"  $ draw (\(l,m,s) -> s)
  , withStrokeColor "green" $ draw (\(l,m,s) -> m)
  , withStrokeColor "red"   $ draw (\(l,m,s) -> l)
  ]
  where
    draw fn = withFillOpacity 0 $ mkLinePath
      [ (fromIntegral (ang-minAng) / maxAng * width, fn val * height)
      | (ang, val) <- Map.toList coneSensitivity
      ]
    width = 100
    height = -50
    maxAng = fromIntegral $ fst (Map.findMax coneSensitivity) - minAng
    minAng = fst (Map.findMin coneSensitivity)
