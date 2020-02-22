#!/usr/bin/env stack
-- stack --resolver lts-13.14 runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module EndScene (endScene) where

import           Control.Lens          ()
import           Control.Monad
import qualified Data.ByteString       as BS
import qualified Data.Map              as Map
import           Data.Monoid
import qualified Data.Text             as T

import           Codec.Picture
import           Codec.Picture.Jpg
import           Codec.Picture.Types
import           Data.Maybe
import           Data.Word
import           Graphics.SvgTree      hiding (Image, imageHeight, imageWidth)
import           Graphics.SvgTree.Memo
import           Numeric
import           Reanimate
import           Reanimate.Animation
import           Reanimate.ColorMap
import           Reanimate.ColorSpace
import           Reanimate.Builtin.Images
import           Reanimate.Constants
import           Reanimate.Effect
import           Reanimate.LaTeX
import           Reanimate.Raster
import           Reanimate.Scene
import           Reanimate.Signal
import           Reanimate.Svg
import           System.IO.Unsafe

endScene :: Animation
endScene = mkAnimation 5 $ const $
  scale 0.5 $ githubIcon
