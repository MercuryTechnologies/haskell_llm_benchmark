module Frequency (frequency) where

import Data.Map (Map, fromListWith)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Parallel (par, pseq)
import Control.DeepSeq (deepseq)

frequency :: Int -> [Text] -> Map Char Int
frequency nWorkers texts = 
    let chunkedTexts = chunkTexts nWorkers texts
        letterCounts = map countLetters chunkedTexts
    in  mergeCounts letterCounts

chunkTexts :: Int -> [Text] -> [[Text]]
chunkTexts nWorkers texts = 
    let chunkSize = (length texts + nWorkers - 1) `div` nWorkers
    in  take nWorkers $ map (take chunkSize) (iterate (drop chunkSize) texts)

countLetters :: [Text] -> Map Char Int
countLetters texts = 
    fromListWith (+) [(c, 1) | text <- texts, c <- T.unpack text]

mergeCounts :: [Map Char Int] -> Map Char Int
mergeCounts counts = 
    fromListWith (+) [(c, count) | m <- counts, (c, count) <- m]
