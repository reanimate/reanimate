module Reanimate.Svg where

import Data.Maybe
import qualified Data.Map as Map
import Control.Lens (over, (^.),set, (.~), (&), (%~) )
import Graphics.Svg

replaceUses :: Document -> Document
replaceUses doc = doc & elements %~ map (mapTree replace)
                      & definitions .~ Map.empty
  where
    replace (UseTree _ Just{}) = error "replaceUses: subtree in use?"
    replace (UseTree use Nothing) =
      case Map.lookup (use^.useName) idMap of
        Nothing -> error $ "Unknown id: " ++ (use^.useName)
        Just tree ->
          GroupTree $
          defaultSvg & groupChildren .~ [tree]
                     & drawAttr .~ (defaultSvg & transform .~ Just [baseToTransformation (use^.useBase)])
    replace x = x
    baseToTransformation (x,y) =
      case (toUserUnit defaultDPI x, toUserUnit defaultDPI y) of
        (Num a, Num b) -> Translate a b
        _              -> TransformUnknown
    defaultDPI = 96
    docTree = GroupTree $ set groupChildren (doc^.elements) defaultSvg
    idMap = foldTree updMap Map.empty docTree `Map.union`
            Map.mapMaybe elementToTree (doc^.definitions)
    updMap m tree =
      case tree^.drawAttr.attrId of
        Nothing -> m
        Just tid -> Map.insert tid tree m
    elementToTree (ElementGeometry t) = Just t
    elementToTree _ = Nothing

docIds :: Document -> [String]
docIds doc = Map.keys idMap ++ Map.keys (doc^.definitions)
  where
    docTree = GroupTree $ set groupChildren (doc^.elements) defaultSvg
    idMap = foldTree updMap Map.empty docTree
    updMap m tree =
      case tree^.drawAttr.attrId of
        Nothing -> m
        Just tid -> Map.insert tid tree m


-- Transform out viewbox. defs and CSS rules are discarded.
unbox :: Document -> Document
unbox doc@Document{_viewBox = Just (minx, minw, _width, _height)} =
  doc & viewBox .~ Nothing
      & width .~ Nothing
      & height .~ Nothing
      & elements .~
        [ GroupTree $ defaultSvg
          & groupChildren .~ doc^.elements
          & drawAttr .~ (defaultSvg & transform .~ Just [Translate (-minx) (-minw)]) ]
unbox doc = doc
