{-|
Copyright   : Written by David Himmelstrup
License     : Unlicense
Maintainer  : lemmih@gmail.com
Stability   : experimental
Portability : POSIX
-}
module Reanimate.Svg.Unuse
  ( replaceUses
  , unbox
  , unboxFit
  , embedDocument
  ) where

import           Control.Lens               ((%~), (&), (.~), (?~), (^.))
import qualified Data.Map                   as Map
import           Data.Maybe                 (fromMaybe)
import           Graphics.SvgTree
import           Reanimate.Constants        (defaultDPI, screenHeight, screenWidth)
import           Reanimate.Svg.Constructors (flipYAxis, mkGroup, scaleXY, translate,
                                             withFillOpacity, withStrokeWidth)

-- | Replace all @<use>@ nodes with their definition.
replaceUses :: Document -> Document
replaceUses doc = doc & documentElements %~ map (mapTree replace)
  where
    replaceDefinition PathTree{}   = None
    replaceDefinition SymbolTree{} = None
    replaceDefinition t            = t

    replace t@DefinitionTree{} = mapTree replaceDefinition t
    replace (UseTree _ Just{}) = error "replaceUses: subtree in use?"
    replace (UseTree use Nothing) =
      case Map.lookup (use^.useName) idMap of
        Nothing -> error $ "Unknown id: " ++ (use^.useName)
        Just (SymbolTree children) -> mapTree replace $
          GroupTree children
          & transform ?~
              fromMaybe [] (use^.transform) ++
              [baseToTransformation (use^.useBase)]
        Just tree -> mapTree replace $
          GroupTree (defaultSvg & groupChildren .~ [tree])
          & transform ?~
              fromMaybe [] (use^.transform) ++
              [baseToTransformation (use^.useBase)]
    replace x = x
    baseToTransformation (x,y) =
      case (toUserUnit defaultDPI x, toUserUnit defaultDPI y) of
        (Num a, Num b) -> Translate a b
        _              -> TransformUnknown
    docTree = mkGroup (doc^.documentElements)
    idMap = foldTree updMap Map.empty docTree
    updMap m tree =
      case tree^.attrId of
        Nothing  -> m
        Just tid -> Map.insert tid tree m

-- FIXME: the viewbox is ignored. Can we use the viewbox as a mask?
-- | Transform out viewbox. Definitions and CSS rules are discarded.
unbox :: Document -> Tree
unbox doc@Document{_documentViewBox = Just (_minx, _miny, _width, _height)} =
  groupTree $ defaultSvg
          & groupChildren .~ doc^.documentElements
unbox doc =
  groupTree $ defaultSvg
    & groupChildren .~ doc^.documentElements

-- | Transform out viewbox and fit image to screen size.
unboxFit :: Document -> Tree
unboxFit doc@Document{_documentViewBox = Just (minx, miny, width, height)} =
  let widthScale = screenWidth/width
      heightScale = screenHeight/height
      scaler = min widthScale heightScale
  in
    scaleXY scaler (-scaler) $
    translate (-minx-width/2) (-miny-height/2) $
    groupTree $ defaultSvg
      & groupChildren .~ doc^.documentElements
unboxFit doc =
  groupTree $ defaultSvg
    & groupChildren .~ doc^.documentElements


-- | Embed 'Document'. This keeps the entire document intact but makes
--   it more difficult to use, say, `Reanimate.Svg.pathify` on it.
embedDocument :: Document -> Tree
embedDocument doc =
  translate (-screenWidth/2) (screenHeight/2) $
  withFillOpacity 1 $
  withStrokeWidth 0 $
  flipYAxis $
  svgTree $ doc & documentWidth .~ Nothing
                & documentHeight .~ Nothing
