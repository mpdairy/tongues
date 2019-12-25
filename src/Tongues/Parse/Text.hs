{-# LANGUAGE TemplateHaskell #-}

module Tongues.Parse.Text where

import Tongues.Prelude hiding (Word)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Char as Char
import Data.Char (isLetter, isSpace, isUpper)

import Data.Attoparsec.Text (parseOnly, Parser)
import qualified Data.Attoparsec.Text as P
import Tongues.Types.Text

-- Divides paragraphs of text into sentences --

-- assumes no whitespace at start, and whitepspace or eof at end
parseWord :: Parser Word
parseWord = do
  pre' <- P.takeWhile (not . isLetter)
  word' <- P.takeWhile isLetter
  post' <- P.takeWhile (not . isSpace)
  return $ Word pre' word' post'


stopChars :: Set Char
stopChars = Set.fromList ['.', '!', '?']

-- this should eventually ignore things like Dr. Mr. Mrs., abbreviations, etc.
-- but it might be fine for german...
isWordEndOfSentence :: Word -> Bool
isWordEndOfSentence = isJust . Text.find (flip Set.member stopChars) . view post

parseSentence :: Parser Sentence
parseSentence = (P.endOfInput >> pure []) <|> do
  w <- parseWord
  case isWordEndOfSentence w of
    False -> (w:) <$> parseSentence
    True -> pure [w]
  

----------------

--caps the second word if the first word is capped
capIfCapped :: Text -> Text -> Text
capIfCapped og w = maybe w identity $ do
  (fl1, _) <- Text.uncons og
  (fl2, t) <- Text.uncons w
  return $ if isUpper fl1 then Text.cons (Char.toUpper fl2) t else w

-- makes second word look like it's in the same context as the first
cloakWord :: Word -> Word -> Word
cloakWord og w = w
  & pre .~ og ^. pre
  & word %~ capIfCapped (og ^. word)
  & post .~ og ^. post
