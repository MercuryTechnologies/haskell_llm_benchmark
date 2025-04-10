module Frequency (frequency) where

import Control.DeepSeq (force)
import Control.Parallel.Strategies (parListChunk, rdeepseq, withStrategy)
import Data.Map  (Map, empty, insertWith, unionWith)
import Data.Text (Text, unpack)

frequency :: Int -> [Text] -> Map Char Int
frequency nWorkers texts = 
  let chunks = withStrategy (parListChunk (max 1 $ length texts `div` nWorkers) rdeepseq) 
                 $ map countLetters texts
  in foldl1 (unionWith (+)) chunks

countLetters :: Text -> Map Char Int
countLetters = foldl (\acc c -> insertWith (+) c 1 acc) empty . unpack
