module Reanimate.ColorSpace where

import qualified Data.ByteString.Lazy       as BS
import           Data.Colour.CIE
import           Data.Colour.CIE.Illuminant (d65)
import           Data.Csv
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Maybe
import qualified Data.Vector                as V
import           Paths_reanimate
import           System.IO.Unsafe

import           Graphics.SvgTree           (Tree)
import           Reanimate.Svg.Constructors

type Nanometer = Integer


lightXYZCoordinates :: Map Nanometer (Double, Double, Double)
lightXYZCoordinates = unsafePerformIO $ do
  dat <- BS.readFile =<< getDataFileName "data/CIExyz.csv"
  case decode NoHeader dat of
    Left err -> error err
    Right vec -> return $ Map.fromList
      [ (nm, (x,y,z)) | (nm,x,y,z) <- V.toList vec, nm <= 700 ]

bigXYZCoordinates :: Map Nanometer (Double, Double, Double)
bigXYZCoordinates = unsafePerformIO $ do
  dat <- BS.readFile =<< getDataFileName "data/CIE_XYZ.csv"
  case decode NoHeader dat of
    Left err -> error err
    Right vec -> return $ Map.fromList
      [ (nm, (x,y,z)) | (nm,x,y,z) <- V.toList vec, nm <= 700 ]

renderXYZCoordinates :: Tree
renderXYZCoordinates =
  withFillOpacity 0 $
  mkLinePath $
  [ (x, 1-y)
  | (_nm, (x,y,_z)) <- Map.toList lightXYZCoordinates
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
  | (_nm, (_l,a,b)) <- Map.toList lightLABCoordinates
  ]

-- (Long, Medium, Short)
coneSensitivity :: Map Nanometer (Double, Double, Double)
coneSensitivity = unsafePerformIO $ do
  dat <- BS.readFile =<< getDataFileName "data/cone_sensitivity_lms.csv"
  case decode NoHeader dat of
    Left err -> error err
    Right vec -> return $ Map.fromList
      [ (nm, (l,m,fromMaybe 0 s)) | (nm,l,m,s) <- V.toList vec, nm <= 700 ]

renderSensitivity :: Tree
renderSensitivity = mkGroup
  [ withStrokeColor "blue"  $ draw (\(_,_,s) -> s)
  , withStrokeColor "green" $ draw (\(_,m,_) -> m)
  , withStrokeColor "red"   $ draw (\(l,_,_) -> l)
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
