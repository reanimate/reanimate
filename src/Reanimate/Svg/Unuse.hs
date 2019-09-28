module Reanimate.Svg.Unuse
  ( replaceUses
  , unbox
  ) where

import           Control.Lens                 ((%~), (&), (.~), (^.))
import qualified Data.Map                     as Map
import           Graphics.SvgTree             hiding (height, line, path, use,
                                               width)
import           Reanimate.Constants
import           Reanimate.Svg.Constructors

replaceUses :: Document -> Document
replaceUses doc = doc & elements %~ map (mapTree replace)
                      & definitions .~ Map.empty
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
                     & transform .~ Just [baseToTransformation (use^.useBase)]
    replace x = x
    baseToTransformation (x,y) =
      case (toUserUnit defaultDPI x, toUserUnit defaultDPI y) of
        (Num a, Num b) -> Translate a b
        _              -> TransformUnknown
    docTree = mkGroup (doc^.elements)
    idMap = foldTree updMap Map.empty docTree `Map.union`
            (doc^.definitions)
    updMap m tree =
      case tree^.attrId of
        Nothing  -> m
        Just tid -> Map.insert tid tree m

-- Transform out viewbox. defs and CSS rules are discarded.
unbox :: Document -> Tree
unbox doc@Document{_viewBox = Just (minx, minw, _width, _height)} =
  GroupTree $ defaultSvg
          & groupChildren .~ doc^.elements
          & transform .~ Just [Translate (-minx) (-minw)]
unbox doc =
  GroupTree $ defaultSvg
    & groupChildren .~ doc^.elements
