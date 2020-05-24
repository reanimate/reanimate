module Reanimate.Svg.Unuse
  ( replaceUses
  , unbox
  , embedDocument
  ) where

import           Control.Lens               ((%~), (&), (.~), (?~), (^.))
import qualified Data.Map                   as Map
import           Graphics.SvgTree           hiding (line, path, use)
import           Reanimate.Constants
import           Reanimate.Svg.Constructors

replaceUses :: Document -> Document
replaceUses doc = doc & elements %~ map (mapTree replace)
  where
    replaceDefinition PathTree{} = None
    replaceDefinition t          = t

    replace t@DefinitionTree{} = mapTree replaceDefinition t
    replace (UseTree _ Just{}) = error "replaceUses: subtree in use?"
    replace (UseTree use Nothing) =
      case Map.lookup (use^.useName) idMap of
        Nothing -> error $ "Unknown id: " ++ (use^.useName)
        Just tree ->
          GroupTree $
          defaultSvg & groupChildren .~ [tree]
                     & transform ?~ [baseToTransformation (use^.useBase)]
    replace x = x
    baseToTransformation (x,y) =
      case (toUserUnit defaultDPI x, toUserUnit defaultDPI y) of
        (Num a, Num b) -> Translate a b
        _              -> TransformUnknown
    docTree = mkGroup (doc^.elements)
    idMap = foldTree updMap Map.empty docTree
    updMap m tree =
      case tree^.attrId of
        Nothing  -> m
        Just tid -> Map.insert tid tree m

-- FIXME: the viewbox is ignored. Can we use the viewbox as a mask?
-- Transform out viewbox. defs and CSS rules are discarded.
unbox :: Document -> Tree
unbox doc@Document{_viewBox = Just (_minx, _minw, _width, _height)} =
  GroupTree $ defaultSvg
          & groupChildren .~ doc^.elements
unbox doc =
  GroupTree $ defaultSvg
    & groupChildren .~ doc^.elements

embedDocument :: Document -> Tree
embedDocument doc =
  translate (-screenWidth/2) (screenHeight/2) $
  withFillOpacity 1 $
  withStrokeWidth 0 $
  flipYAxis $
  SvgTree $ doc & width .~ Nothing
                & height .~ Nothing
