module Reanimate.Svg.Constructors where

import           Codec.Picture                (PixelRGBA8 (..))
import           Control.Lens                 ((&), (.~))
import           Data.Attoparsec.Text         (parseOnly)
import qualified Data.Map                     as Map
import qualified Data.Text                    as T
import           Graphics.SvgTree             hiding (height, line, path, use,
                                               width)
import           Graphics.SvgTree.NamedColors
import           Graphics.SvgTree.PathParser
import           Linear.V2                    hiding (angle)
import           Reanimate.Constants
import           Reanimate.Svg.BoundingBox

withTransformations :: [Transformation] -> Tree -> Tree
withTransformations transformations t =
  mkGroup [t] & transform .~ Just transformations

translate :: Double -> Double -> Tree -> Tree
translate x y = withTransformations [Translate x y]

rotate :: Double -> Tree -> Tree
rotate a = withTransformations [Rotate a Nothing]

rotateAround :: Double -> RPoint -> Tree -> Tree
rotateAround a (V2 x y) = withTransformations [Rotate a (Just (x,y))]

rotateAroundCenter :: Double -> Tree -> Tree
rotateAroundCenter a t =
    rotateAround a (V2 (x+w/h) (y+h/2)) t
  where
    (x,y,w,h) = boundingBox t

scale :: Double -> Tree -> Tree
scale a = withTransformations [Scale a Nothing]

scaleToSize :: Double -> Double -> Tree -> Tree
scaleToSize w h t =
    scaleXY (w/w') (h/h') t
  where
    (_x, _y, w', h') = boundingBox t

scaleToWidth :: Double -> Tree -> Tree
scaleToWidth w t =
    scale (w/w') t
  where
    (_x, _y, w', _h') = boundingBox t

scaleToHeight :: Double -> Tree -> Tree
scaleToHeight h t =
    scale (h/h') t
  where
    (_x, _y, _w', h') = boundingBox t

scaleXY :: Double -> Double -> Tree -> Tree
scaleXY x y = withTransformations [Scale x (Just y)]

center :: Tree -> Tree
center t = translate (-x-w/2) (-y-h/2) t
  where
    (x, y, w, h) = boundingBox t

centerX :: Tree -> Tree
centerX t = translate (-x-w/2) 0 t
  where
    (x, _y, w, _h) = boundingBox t

centerY :: Tree -> Tree
centerY t = translate 0 (-y-h/2) t
  where
    (_x, y, _w, h) = boundingBox t

mkColor :: String -> Texture
mkColor name =
  case Map.lookup (T.pack name) svgNamedColors of
    Nothing -> ColorRef (PixelRGBA8 240 248 255 255)
    Just c  -> ColorRef c

withStrokeColor :: String -> Tree -> Tree
withStrokeColor color = strokeColor .~ pure (mkColor color)

withStrokeLineJoin :: LineJoin -> Tree -> Tree
withStrokeLineJoin ljoin = strokeLineJoin .~ pure ljoin

withFillColor :: String -> Tree -> Tree
withFillColor color = fillColor .~ pure (mkColor color)

withFillColorPixel :: PixelRGBA8 -> Tree -> Tree
withFillColorPixel color = fillColor .~ pure (ColorRef color)

withFillOpacity :: Double -> Tree -> Tree
withFillOpacity opacity = fillOpacity .~ Just (realToFrac opacity)

withGroupOpacity :: Double -> Tree -> Tree
withGroupOpacity opacity = groupOpacity .~ Just (realToFrac opacity)

withStrokeWidth :: Double -> Tree -> Tree
withStrokeWidth width = strokeWidth .~ pure (Num width)

withClipPathRef :: ElementRef -> Tree -> Tree
withClipPathRef ref = clipPathRef .~ pure ref

withId :: String -> Tree -> Tree
withId idTag = attrId .~ Just idTag

mkRect :: Double -> Double -> Tree
mkRect width height = translate (-width/2) (-height/2) $ RectangleTree $ defaultSvg
  & rectUpperLeftCorner .~ (Num 0, Num 0)
  & rectWidth .~ Just (Num width)
  & rectHeight .~ Just (Num height)

mkCircle :: Double -> Tree
mkCircle radius = CircleTree $ defaultSvg
  & circleCenter .~ (Num 0, Num 0)
  & circleRadius .~ Num radius

mkLine :: (Double,Double) -> (Double, Double) -> Tree
mkLine (x1,y1) (x2,y2) = LineTree $ defaultSvg
  & linePoint1 .~ (Num x1, Num y1)
  & linePoint2 .~ (Num x2, Num y2)

mkGroup :: [Tree] -> Tree
mkGroup forest = GroupTree $ defaultSvg
  & groupChildren .~ forest

mkDefinitions :: [Tree] -> Tree
mkDefinitions forest = DefinitionTree $ defaultSvg
  & groupChildren .~ forest

mkUse :: String -> Tree
mkUse name = UseTree (defaultSvg & useName .~ name) Nothing

mkClipPath :: String -> [Tree] -> Tree
mkClipPath idTag forest = withId idTag $ ClipPathTree $ (defaultSvg
  & clipPathContent .~ forest)

mkPathString :: String -> Tree
mkPathString = mkPathText . T.pack

mkPathText :: T.Text -> Tree
mkPathText str =
  case parseOnly pathParser str of
    Left err   -> error err
    Right cmds -> PathTree $ defaultSvg & pathDefinition .~ cmds

mkLinePath :: [(Double, Double)] -> Tree
mkLinePath [] = mkGroup []
mkLinePath ((startX, startY):rest) =
    PathTree $ defaultSvg & pathDefinition .~ cmds
  where
    cmds = [ MoveTo OriginAbsolute [V2 startX startY]
           , LineTo OriginAbsolute [ V2 x y | (x, y) <- rest ] ]

mkBackground :: String -> Tree
mkBackground color = withFillColor color $ mkRect screenWidth screenHeight

mkBackgroundPixel :: PixelRGBA8 -> Tree
mkBackgroundPixel pixel =
    withFillColorPixel pixel $ mkRect screenWidth screenHeight

gridLayout :: [[Tree]] -> Tree
gridLayout rows = mkGroup
    [ translate (-screenWidth/2+colSep*(nCol))
                (screenHeight/2-rowSep*(nRow))
      elt
    | (nRow, col) <- zip [1..] rows
    , let nCols = length col
          colSep = screenWidth / fromIntegral (nCols+1)
    , (nCol, elt) <- zip [1..] col ]
  where
    rowSep = screenHeight / fromIntegral (nRows+1)
    nRows = length rows
