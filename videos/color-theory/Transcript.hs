module Transcript
  ( V.Transcript(..)
  , V.TWord(..)
  , transcript
  , findWord
  , findWords
  , V.annotateWithTranscript
  ) where

import qualified Reanimate.Voice as V


transcript :: V.Transcript
transcript = V.loadTranscript "SCRIPT.txt"

findWord = V.findWord transcript
findWords = V.findWords transcript