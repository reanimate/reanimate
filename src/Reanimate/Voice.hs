{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
module Reanimate.Voice
  ( Transcript(..)
  , TWord(..)
  , loadTranscript  -- :: FilePath -> Transcript
  , fakeTranscript  -- :: T.Text -> Transcript
  , splitTranscript -- :: Transcript -> SVG -> [(SVG, TWord)]
  )
where

import           Data.Aeson
import           Data.Char
import           System.IO.Unsafe                         ( unsafePerformIO )
import           System.Directory
import           System.FilePath
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Reanimate.Animation                      ( SVG )
import           Reanimate.Svg                            ( mkGroup
                                                          , svgGlyphs
                                                          , simplify
                                                          )

data Transcript = Transcript
  { transcriptText  :: T.Text
  , transcriptWords :: [TWord]
  } deriving (Show)

instance FromJSON Transcript where
  parseJSON =
    withObject "transcript" $ \o -> Transcript <$> o .: "transcript" <*> o .: "words"

data TWord = TWord
  { wordAligned     :: T.Text
  , wordCase        :: T.Text
  , wordStart       :: Double
  , wordStartOffset :: Int
  , wordEnd         :: Double
  , wordEndOffset   :: Int
  , wordPhones      :: [Phone]
  , wordReference   :: T.Text
  } deriving (Show)

instance FromJSON TWord where
  parseJSON = withObject "word" $ \o ->
    TWord
      <$> o
      .:? "alignedWord"
      .!= T.empty
      <*> o
      .:  "case"
      <*> o
      .:? "start"
      .!= 0
      <*> o
      .:  "startOffset"
      <*> o
      .:? "end"
      .!= 0
      <*> o
      .:  "endOffset"
      <*> o
      .:? "phones"
      .!= []
      <*> o
      .:  "word"

data Phone = Phone
  { phoneDuration :: Double
  , phoneType     :: T.Text
  } deriving (Show)

instance FromJSON Phone where
  parseJSON = withObject "phone" $ \o -> Phone <$> o .: "duration" <*> o .: "phone"

findWord :: [String] -> String -> TWord
findWord keys w = case findWords keys w of
  [] -> error $ "Word not in transcript: " ++ show (keys, w)
  (tword:_) -> tword

findWords :: [String] -> String -> [TWord]
findWords = undefined

loadTranscript :: FilePath -> Transcript
loadTranscript path = unsafePerformIO $ do
  hasJSON <- doesFileExist jsonPath
  if hasJSON
    then do
      mbT <- decodeFileStrict jsonPath
      case mbT of
        Nothing -> error "bad json"
        Just t  -> pure t
    else fakeTranscript <$> T.readFile txtPath
 where
  base     = takeBaseName path
  jsonPath = base <.> "json"
  txtPath  = base <.> "txt"

data Token = TokenWord Int Int T.Text | TokenComma | TokenPeriod | TokenParagraph
  deriving (Show)

lexText :: T.Text -> [Token]
lexText = worker 0
 where
  worker offset txt = case T.uncons txt of
    Nothing -> []
    Just (c, cs)
      | isSpace c
      -> let (w, rest) = T.span (== '\n') txt
         in  if T.length w >= 2
               then TokenParagraph : worker (offset + T.length w) rest
               else worker (offset + 1) cs
      | c == '.'
      -> TokenPeriod : worker (offset + 1) cs
      | c == ','
      -> TokenComma : worker (offset + 1) cs
      | isAlphaNum c
      -> let (w, rest) = T.span isAlphaNum txt
             newOffset = offset + T.length w
         in  TokenWord offset newOffset w : worker newOffset rest
      | otherwise
      -> worker (offset + 1) cs

fakeTranscript :: T.Text -> Transcript
fakeTranscript input = Transcript { transcriptText  = input
                                  , transcriptWords = worker 0 (lexText input)
                                  }
 where
  worker _now []             = []
  worker now  (token : rest) = case token of
    TokenWord start end w ->
      let duration = realToFrac (end - start) * 0.1
      in  TWord { wordAligned     = T.toLower w
                , wordCase        = "success"
                , wordStart       = now
                , wordStartOffset = start
                , wordEnd         = now + duration
                , wordEndOffset   = end
                , wordPhones      = []
                , wordReference   = w
                }
            : worker (now + duration) rest
    TokenComma     -> worker (now + commaPause) rest
    TokenPeriod    -> worker (now + periodPause) rest
    TokenParagraph -> worker (now + paragraphPause) rest
  paragraphPause = 0.5
  commaPause     = 0.1
  periodPause    = 0.2

splitTranscript :: Transcript -> SVG -> [(SVG, TWord)]
splitTranscript Transcript {..} rendered
  | T.length textSymbols /= length gls
  = error $ "Bad size: " ++ show (T.length textSymbols, length gls)
  | otherwise
  = [ ( mkGroup $ take (wordEndOffset - wordStartOffset) $ drop
        (wordStartOffset - spaces)
        gls
      , tword
      )
    | tword@TWord {..} <- transcriptWords
    , let spaces = nSpaces wordStartOffset
    ]
 where
  nSpaces limit = T.length (T.filter isSpace (T.take limit transcriptText))
  textSymbols = T.filter (not . isSpace) transcriptText
  gls         = [ ctx g | (ctx, _attr, g) <- svgGlyphs $ simplify rendered ]

