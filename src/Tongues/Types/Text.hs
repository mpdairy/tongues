{-# LANGUAGE TemplateHaskell #-}

module Tongues.Types.Text where

import Tongues.Prelude hiding (Word)


data Word = Word { _pre :: Text
                 , _word :: Text
                 , _post :: Text
                 } deriving (Eq, Ord, Show)

type Sentence = [Word]

type Paragraph = [Sentence]

$(makeFieldsNoPrefix ''Word)
