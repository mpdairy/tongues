{-# LANGUAGE TemplateHaskell #-}
module Tongues.Learn.Triad where

import Tongues.Prelude
import Tongues.Types.Text (Word, Sentence)
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Tongues.Types.Text as T

data Triad = Triad
  { _word :: Word
  , _candidates :: (Text, Text, Text)
  } deriving (Eq, Ord, Show)

$(makeFieldsNoPrefix ''Triad)

type TriadSentence = [Triad]

type TriadLesson = [TriadSentence]

getRandomElementFromSet :: Set a -> IO (Maybe a)
getRandomElementFromSet s
  | sz == 0 = return Nothing
  | otherwise = randomRIO (0, sz) >>= return . Just . flip Set.elemAt s
  where
    sz = Set.size s

getRandomElementFromList :: [a] -> IO (Maybe a)
getRandomElementFromList xs
  | sz == 0 = return Nothing
  | otherwise = randomRIO (0, sz) >>= return . Just . (xs !!)
  where
    sz = length xs

getRandomElementFromList' :: (IsString e, MonadError e m, MonadIO m) => [a] -> m a
getRandomElementFromList' xs
  | sz == 0 = throwError "List has no elements"
  | otherwise = liftIO (randomRIO (0, sz)) >>= return . (xs !!)
  where
    sz = length xs


getWordTextsFromSentence :: Sentence -> Set Text
getWordTextsFromSentence = foldr (\w s -> Set.insert (w ^. T.word) s) Set.empty

getAllWordTexts :: [Sentence] -> Set Text
getAllWordTexts = foldr (\ws s -> Set.union (getWordTextsFromSentence ws) s) Set.empty

-- makeTriad :: Set Text -> IO Triad
-- makeTriad wordbag = do
  

-- makeLesson :: [Sentence] -> IO
