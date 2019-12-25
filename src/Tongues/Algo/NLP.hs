module Tongues.Algo.NLP where

import Tongues.Prelude

import qualified Data.Text as Text
import qualified Data.Map as Map

data BChar = Start
           | BChar Char
           | Stop
           deriving (Eq, Ord, Show)

type Bigram = (BChar, BChar)

bigramCounts :: [Bigram] -> Map Bigram Int
bigramCounts = foldr f Map.empty where
  f bg m = Map.insertWith (+) bg 1 m

asBigrams :: Text -> [Bigram]
asBigrams t = zip t' (drop 1 t')
  where
    t' = Start : (BChar <$> Text.unpack t) <> [Stop]

subtractBigramCountMap :: Map Bigram Int -> Map Bigram Int -> Map Bigram Int
subtractBigramCountMap m1 = Map.foldrWithKey f m1
  where
    f :: Bigram -> Int -> Map Bigram Int -> Map Bigram Int
    f bg c m = Map.insertWith (+) bg (0 - c) m

bigramWordSimiliary :: Text -> Text -> Double
bigramWordSimiliary t1 t2 = 1.0 - total mdiff / (total m1 + total m2) 
  where
    f = bigramCounts . asBigrams . Text.toLower
    m1 = f t1
    m2 = f t2
    mdiff = abs <$> subtractBigramCountMap m1 m2
    total = fromIntegral . foldr (+) 0


