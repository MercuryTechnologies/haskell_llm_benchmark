module Frequency (frequency) where

import Data.Char (isLetter, toLower)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Control.Parallel.Strategies (parBuffer, rdeepseq, using)

-- | Count letter frequency in parallel
frequency :: Int -> [Text] -> Map Char Int
frequency nWorkers texts
  | nWorkers <= 0 = Map.empty
  | null texts = Map.empty
  | otherwise = 
      -- Divide work into chunks based on number of workers
      let chunks = splitIntoChunks nWorkers texts
          -- Process each chunk and get frequency map
          chunkMaps = map processChunk chunks
          -- Combine results using parallel evaluation
          result = chunkMaps `using` parBuffer nWorkers rdeepseq
      in Map.unionsWith (+) result

-- | Split texts into roughly equal chunks
splitIntoChunks :: Int -> [Text] -> [[Text]]
splitIntoChunks n xs
  | n <= 0 = []
  | n >= length xs = map (:[]) xs
  | otherwise =
      let chunkSize = ceiling (fromIntegral (length xs) / fromIntegral n)
      in chunk chunkSize xs
  where
    chunk _ [] = []
    chunk size ys = take size ys : chunk size (drop size ys)

-- | Process a chunk of texts and return frequency map
processChunk :: [Text] -> Map Char Int
processChunk = foldr (Map.unionWith (+) . processText) Map.empty

-- | Process a single text and count letter frequencies
processText :: Text -> Map Char Int
processText = T.foldr countLetter Map.empty
  where
    countLetter c acc
      | isLetter c = Map.insertWith (+) (toLower c) 1 acc
      | otherwise  = acc
