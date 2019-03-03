{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PackageImports            #-}
{-# LANGUAGE TypeFamilies              #-}
module Reanimate.Diagrams
  ( renderDiagram
  , SvgDiagram
  ) where

import qualified Data.ByteString.Lazy    as BL
import qualified Diagrams.Backend.SVG    as D
import qualified Diagrams.Core.Compile   as D
import qualified Diagrams.Core.Types     as D
import qualified Diagrams.Size           as D
import           "svg-tree" Graphics.Svg (Document (..), Tree (..), defaultSvg,
                                          elements, loadSvgFile, parseSvgFile,
                                          xmlOfDocument)
import qualified Graphics.Svg.Core       as Svg
import           Linear.V2
import           Reanimate.Svg           (unbox)

import           Diagrams.Prelude
import qualified Diagrams.Prelude        as D

renderDiagram :: SvgDiagram -> Tree
renderDiagram d =
    case parseSvgFile "" (BL.toStrict $ Svg.renderBS (renderDia D.SVG opts d)) of
      Nothing  -> error "Malformed svg"
      Just svg -> unbox svg
  where
    -- opts = SVGOptions (mkSizeSpec (V2 Nothing Nothing)) Nothing "" [] False
    opts = D.SVGOptions absolute Nothing "" [] False

type SvgDiagram = D.Diagram D.SVG
