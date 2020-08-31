{-# LANGUAGE OverloadedStrings #-}
module Animation where
import Reanimate
import Reanimate.Builtin.Documentation
import Reanimate.Builtin.Images
import Reanimate.Builtin.CirclePlot
import Reanimate.Builtin.TernaryPlot
import Reanimate.Morph.Common
import Reanimate.Morph.Linear
import Reanimate.Scene
import Control.Lens
import Codec.Picture.Types
animation = docEnv drawCircle