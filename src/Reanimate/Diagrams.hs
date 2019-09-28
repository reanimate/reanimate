{-# LANGUAGE OverloadedStrings #-}
module Reanimate.Diagrams
  ( renderDiagram
  , SvgDiagram
  ) where

import qualified Data.ByteString.Lazy as BL
import qualified Diagrams.Backend.SVG as D
import qualified Diagrams.Core.Types  as D
import           Diagrams.Prelude
import qualified Graphics.Svg.Core    as Svg
import           Graphics.SvgTree     (Tree (..), parseSvgFile)
import           Reanimate.Svg.Unuse  (unbox)

renderDiagram :: SvgDiagram -> Tree
renderDiagram d =
    case parseSvgFile "" (BL.toStrict $ Svg.renderBS (renderDia D.SVG opts d)) of
      Nothing  -> error "Malformed svg"
      Just svg -> unbox svg
  where
    -- opts = SVGOptions (mkSizeSpec (V2 Nothing Nothing)) Nothing "" [] False
    opts = D.SVGOptions absolute Nothing "" [] False

type SvgDiagram = D.Diagram D.SVG
