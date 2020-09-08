{-| Functions for creating basic SVG elements and applying transformations to them. -}
module Reanimate.Svg.Constructors
  ( -- * Primitive shapes
    mkCircle
  , mkEllipse
  , mkRect
  , mkLine
  , mkPath
  , mkPathString
  , mkPathText
  , mkLinePath
  , mkLinePathClosed
  , mkClipPath
  , mkText
  -- * Grouping shapes and definitions
  , mkGroup
  , mkDefinitions
  , mkUse
  -- * Attributes
  , withId
  , withStrokeColor
  , withStrokeColorPixel
  , withStrokeDashArray
  , withStrokeLineJoin
  , withFillColor
  , withFillColorPixel
  , withFillOpacity
  , withGroupOpacity
  , withStrokeWidth
  , withClipPathRef
  -- * Transformations
  , center
  , centerX
  , centerY
  , centerUsing
  , translate
  , rotate
  , rotateAroundCenter
  , rotateAround
  , scale
  , scaleToSize
  , scaleToWidth
  , scaleToHeight
  , scaleXY
  , flipXAxis
  , flipYAxis
  , aroundCenter
  , aroundCenterX
  , aroundCenterY
  , withTransformations
  , withViewBox
  -- * Other
  , mkColor
  , mkBackground
  , mkBackgroundPixel
  , gridLayout

  ) where

import           Codec.Picture                (PixelRGBA8 (..))
import           Control.Lens                 ((&), (.~), (?~))
import           Data.Attoparsec.Text         (parseOnly)
import qualified Data.Map                     as Map
import qualified Data.Text                    as T
import           Graphics.SvgTree
import           Graphics.SvgTree.NamedColors
import           Graphics.SvgTree.PathParser
import           Linear.V2                    hiding (angle)
import           Reanimate.Constants
import           Reanimate.Svg.BoundingBox

-- | Apply list of transformations to given image.
withTransformations :: [Transformation] -> Tree -> Tree
withTransformations transformations t =
  mkGroup [t] & transform ?~ transformations

-- | @translate x y image@ moves the @image@ by @x@ along X-axis and by @y@ along Y-axis.
translate :: Double -> Double -> Tree -> Tree
translate x y = withTransformations [Translate x y]

-- | @rotate angle image@ rotates the @image@ around origin @(0,0)@ counterclockwise by @angle@
--   given in degrees.
rotate :: Double -> Tree -> Tree
rotate a = withTransformations [Rotate a Nothing]

-- | @rotate angle point image@ rotates the @image@ around given @point@ counterclockwise by
--   @angle@ given in degrees.
rotateAround :: Double -> RPoint -> Tree -> Tree
rotateAround a (V2 x y) = withTransformations [Rotate a (Just (x,y))]

-- | @rotate angle image@ rotates the @image@ around the center of its bounding box counterclockwise
--   by @angle@ given in degrees.
rotateAroundCenter :: Double -> Tree -> Tree
rotateAroundCenter a t =
    rotateAround a (V2 (x+w/2) (y+h/2)) t
  where
    (x,y,w,h) = boundingBox t

-- | @aroundCenter f image@ first moves the image so the center of its bounding box is at the origin
--   @(0, 0)@, applies transformation @f@ to it and then moves the transformed image back to its
--   original position.
aroundCenter :: (Tree -> Tree) -> Tree -> Tree
aroundCenter fn t =
    translate (-offsetX) (-offsetY) $ fn $ translate offsetX offsetY t
  where
    offsetX = -x-w/2
    offsetY = -y-h/2
    (x,y,w,h) = boundingBox t

-- | Same as 'aroundCenter' but only for the Y-axis.
aroundCenterY :: (Tree -> Tree) -> Tree -> Tree
aroundCenterY fn t =
    translate 0 (-offsetY) $ fn $ translate 0 offsetY t
  where
    offsetY = -y-h/2
    (_x,y,_w,h) = boundingBox t

-- | Same as 'aroundCenter' but only for the X-axis.
aroundCenterX :: (Tree -> Tree) -> Tree -> Tree
aroundCenterX fn t =
    translate (-offsetX) 0 $ fn $ translate offsetX 0 t
  where
    offsetX = -x-w/2
    (x,_y,w,_h) = boundingBox t

-- | Scale the image uniformly by given factor along both X and Y axes.
-- For example @scale 2 image@  makes the image twice as large, while @scale 0.5 image@ makes it
-- half the original size. Negative values are also allowed, and lead to flipping the image along
-- both X and Y axes.
scale :: Double -> Tree -> Tree
scale a = withTransformations [Scale a Nothing]

-- | @scaleToSize width height@ resizes the image so that its bounding box has corresponding @width@
--   and @height@.
scaleToSize :: Double -> Double -> Tree -> Tree
scaleToSize w h t =
    scaleXY (w/w') (h/h') t
  where
    (_x, _y, w', h') = boundingBox t

-- | @scaleToWidth width@ scales the image so that the width of its bounding box ends up having
--   given @width@.
scaleToWidth :: Double -> Tree -> Tree
scaleToWidth w t =
    scale (w/w') t
  where
    (_x, _y, w', _h') = boundingBox t

-- | @scaleToHeight height@ scales the image so that the height of its bounding box ends up having
--   given @height@.
scaleToHeight :: Double -> Tree -> Tree
scaleToHeight h t =
    scale (h/h') t
  where
    (_x, _y, _w', h') = boundingBox t

-- | Similar to 'scale', except scale factors for X and Y axes are specified separately.
scaleXY :: Double -> Double -> Tree -> Tree
scaleXY x y = withTransformations [Scale x (Just y)]


-- | Flip the image along vertical axis so that what was on the right will end up on left and vice
--   versa.
flipXAxis :: Tree -> Tree
flipXAxis = scaleXY (-1) 1

-- | Flip the image along horizontal so that what was on the top will end up in the bottom and vice
--   versa.
flipYAxis :: Tree -> Tree
flipYAxis = scaleXY 1 (-1)

-- | Translate given image so that the center of its bouding box coincides with coordinates
--   @(0, 0)@.
center :: Tree -> Tree
center t = centerUsing t t

-- | Translate given image so that the X-coordinate of the center of its bouding box is 0.
centerX :: Tree -> Tree
centerX t = translate (-x-w/2) 0 t
  where
    (x, _y, w, _h) = boundingBox t

-- | Translate given image so that the Y-coordinate of the center of its bouding box is 0.
centerY :: Tree -> Tree
centerY t = translate 0 (-y-h/2) t
  where
    (_x, y, _w, h) = boundingBox t

-- | Center the second argument using the bounding-box of the first.
centerUsing :: Tree -> Tree -> Tree
centerUsing a = translate (-x-w/2) (-y-h/2)
  where
    (x, y, w, h) = boundingBox a

-- | Create 'Texture' based on SVG color name.
--   See <https://en.wikipedia.org/wiki/Web_colors#X11_color_names> for the list of available names.
--   If the provided name doesn't correspond to valid SVG color name, white-ish color is used.
mkColor :: String -> Texture
mkColor name =
  case Map.lookup (T.pack name) svgNamedColors of
    Nothing -> ColorRef (PixelRGBA8 240 248 255 255)
    Just c  -> ColorRef c

-- | See <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/stroke>
withStrokeColor :: String -> Tree -> Tree
withStrokeColor color = strokeColor .~ pure (mkColor color)

-- | See <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/stroke>
withStrokeColorPixel :: PixelRGBA8 -> Tree -> Tree
withStrokeColorPixel color = strokeColor .~ pure (ColorRef color)

-- | See <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/stroke-dasharray>
withStrokeDashArray :: [Double] -> Tree -> Tree
withStrokeDashArray arr = strokeDashArray .~ pure (map Num arr)

-- | See <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/stroke-linejoin>
withStrokeLineJoin :: LineJoin -> Tree -> Tree
withStrokeLineJoin ljoin = strokeLineJoin .~ pure ljoin

-- | See <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/fill>
withFillColor :: String -> Tree -> Tree
withFillColor color = fillColor .~ pure (mkColor color)

-- | See <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/fill>
withFillColorPixel :: PixelRGBA8 -> Tree -> Tree
withFillColorPixel color = fillColor .~ pure (ColorRef color)

-- | See <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/fill-opacity>
withFillOpacity :: Double -> Tree -> Tree
withFillOpacity opacity = fillOpacity ?~ realToFrac opacity

-- | See <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/opacity>
withGroupOpacity :: Double -> Tree -> Tree
withGroupOpacity opacity = groupOpacity ?~ realToFrac opacity

-- | See <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/stroke-width>
withStrokeWidth :: Double -> Tree -> Tree
withStrokeWidth width = strokeWidth .~ pure (Num width)

-- | See <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/clip-path>
withClipPathRef :: ElementRef -- ^ Reference to clip path defined previously (e.g. by 'mkClipPath')
                -> Tree -- ^ Image that will be clipped by the referenced clip path
                -> Tree
withClipPathRef ref sub = mkGroup [sub] & clipPathRef .~ pure ref

-- | Assigns ID attribute to given image.
withId :: String -> Tree -> Tree
withId idTag = attrId ?~ idTag

-- | @mkRect width height@ creates a rectangle with given @with@ and @height@, centered at @(0, 0)@.
-- See <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/rect>
mkRect :: Double -> Double -> Tree
mkRect width height = translate (-width/2) (-height/2) $ rectangleTree $ defaultSvg
  & rectUpperLeftCorner .~ (Num 0, Num 0)
  & rectWidth ?~ Num width
  & rectHeight ?~ Num height

-- | Create a circle with given radius, centered at @(0, 0)@.
-- See <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/circle>
mkCircle :: Double -> Tree
mkCircle radius = circleTree $ defaultSvg
  & circleCenter .~ (Num 0, Num 0)
  & circleRadius .~ Num radius

-- | Create an ellipse given X-axis radius, and Y-axis radius, with center at @(0, 0)@.
-- See <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/ellipse>
mkEllipse :: Double -> Double -> Tree
mkEllipse rx ry = ellipseTree $ defaultSvg
  & ellipseCenter .~ (Num 0, Num 0)
  & ellipseXRadius .~ Num rx
  & ellipseYRadius .~ Num ry

-- | Create a line segment between two points given by their @(x, y)@ coordinates.
-- See <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/line>
mkLine :: (Double,Double) -> (Double, Double) -> Tree
mkLine (x1,y1) (x2,y2) = lineTree $ defaultSvg
  & linePoint1 .~ (Num x1, Num y1)
  & linePoint2 .~ (Num x2, Num y2)

-- | Merges multiple images into one.
-- See <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/g>
mkGroup :: [Tree] -> Tree
mkGroup forest = groupTree $ defaultSvg
  & groupChildren .~ forest

-- | Create definition of graphical objects that can be used at later time.
--   See <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/defs>
mkDefinitions :: [Tree] -> Tree
mkDefinitions forest = definitionTree $ defaultSvg
  & groupChildren .~ forest

-- | Create an element by referring to existing element defined previously.
-- For example you can create a graphical element, assign ID to it using 'withId', wrap it in
-- 'mkDefinitions' and then use it via @use "myId"@.
-- See <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/use>
mkUse :: String -> Tree
mkUse name = useTree (defaultSvg & useName .~ name)

-- | A clip path restricts the region to which paint can be applied.
-- See <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/clipPath>
mkClipPath :: String  -- ^ ID of the clip path, which can then be referred to by other elements
                      --   using 'withClipPathRef'.
           -> [Tree] -- ^ List of shapes that will determine the final shape of the clipping region
           -> Tree
mkClipPath idTag forest = withId idTag $ clipPathTree $ defaultSvg
  & clipPathContent .~ forest

-- | Create a path from the list of path commands.
--   See <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/d#Path_commands>
mkPath :: [PathCommand] -> Tree
mkPath cmds = pathTree $ defaultSvg & pathDefinition .~ cmds

-- | Similar to 'mkPathText', but taking SVG path command as a String.
mkPathString :: String -> Tree
mkPathString = mkPathText . T.pack

-- | Create path from textual representation of SVG path command.
--   If the text doesn't represent valid path command, this function fails with 'Prelude.error'.
--   Use 'mkPath' for type safe way of creating paths.
mkPathText :: T.Text -> Tree
mkPathText str =
  case parseOnly pathParser str of
    Left err   -> error err
    Right cmds -> mkPath cmds

-- | Create a path from a list of @(x, y)@ coordinates of points along the path.
mkLinePath :: [(Double, Double)] -> Tree
mkLinePath [] = mkGroup []
mkLinePath ((startX, startY):rest) =
    pathTree $ defaultSvg & pathDefinition .~ cmds
  where
    cmds = [ MoveTo OriginAbsolute [V2 startX startY]
           , LineTo OriginAbsolute [ V2 x y | (x, y) <- rest ] ]

-- | Create a path from a list of @(x, y)@ coordinates of points along the path.
mkLinePathClosed :: [(Double, Double)] -> Tree
mkLinePathClosed [] = mkGroup []
mkLinePathClosed ((startX, startY):rest) =
    pathTree $ defaultSvg & pathDefinition .~ cmds
  where
    cmds = [ MoveTo OriginAbsolute [V2 startX startY]
           , LineTo OriginAbsolute [ V2 x y | (x, y) <- rest ]
           , EndPath ]

-- | Rectangle with a uniform color and the same size as the screen.
--
--   Example:
--
-- @
-- 'Reanimate.animate' $ 'const' $ 'mkBackground' "yellow"
-- @
--
--   <<docs/gifs/doc_mkBackground.gif>>
mkBackground :: String -> Tree
mkBackground color = withFillOpacity 1 $  withStrokeWidth 0 $
  withFillColor color $ mkRect screenWidth screenHeight

-- | Rectangle with a uniform color and the same size as the screen.
mkBackgroundPixel :: PixelRGBA8 -> Tree
mkBackgroundPixel pixel =
    withFillOpacity 1 $ withStrokeWidth 0 $
    withFillColorPixel pixel $ mkRect screenWidth screenHeight

-- | Take list of rows, where each row consists of number of images and display them in regular
--   grid structure.
--   All rows will get equal amount of vertical space.
--   The images within each row will get equal amount of horizontal space, independent of the other
--   rows. Each row can contain different number of cells.
gridLayout :: [[Tree]] -> Tree
gridLayout rows = mkGroup
    [ translate (-screenWidth/2+colSep*nCol + colSep*0.5)
                (screenHeight/2-rowSep*nRow - rowSep*0.5)
      elt
    | (nRow, row) <- zip [0..] rows
    , let nCols = length row
          colSep = screenWidth / fromIntegral nCols
    , (nCol, elt) <- zip [0..] row ]
  where
    rowSep = screenHeight / fromIntegral nRows
    nRows = length rows

-- | Insert a native text object anchored at the middle.
--
--   Example:
--
-- @
-- 'Reanimate.mkAnimation' 2 $ \\t -> 'scale' 2 $ 'withStrokeWidth' 0.05 $ 'mkText' (T.take (round $ t*15) "text")
-- @
--
--   <<docs/gifs/doc_mkText.gif>>
mkText :: T.Text -> Tree
mkText str =
  flipYAxis
  (TextTree Nothing $ defaultSvg
    & textRoot .~ span_
    & fontSize .~ pure (Num 2))
    & textAnchor .~ pure TextAnchorMiddle
    -- Note: TextAnchorMiddle is placed on the 'flipYAxis' group such that it can easily
    -- be overwritten by the user.
  where
    span_ = defaultSvg & spanContent .~ [SpanText str]

-- | Switch from the default viewbox to a custom viewbox. Nesting custom viewboxes is
--   unlikely to give good results. If you need nested custom viewboxes, you will have
--   to configure them by hand.
--
--   The viewbox argument is (min-x, min-y, width, height).
--
--   Example:
--
-- @
-- 'withViewBox' (0,0,1,1) $ 'mkBackground' "yellow"
-- @
--
--   <<docs/gifs/doc_withViewBox.gif>>
withViewBox :: (Double, Double, Double, Double) -> Tree -> Tree
withViewBox vbox child = translate (-screenWidth/2) (-screenHeight/2) $
  svgTree Document
  { _documentViewBox = Just vbox
  , _documentWidth = Just (Num screenWidth)
  , _documentHeight = Just (Num screenHeight)
  , _documentElements = [child]
  , _documentDescription = ""
  , _documentLocation = ""
  , _documentAspectRatio = PreserveAspectRatio False AlignNone Nothing
  }
