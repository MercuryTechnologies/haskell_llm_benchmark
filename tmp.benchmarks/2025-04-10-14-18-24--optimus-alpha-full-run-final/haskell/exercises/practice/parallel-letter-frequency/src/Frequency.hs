module Frequency (frequency) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isLetter, toLower)

-- Count the frequency of letters in a single Text
letterFreq :: Text -> Map Char Int
letterFreq = T.foldl' go Map.empty
  where
    go acc c
      | isLetter c = Map.insertWith (+) (toLower c) 1 acc
      | otherwise  = acc

-- Merge two frequency maps
mergeFreq :: Map Char Int -> Map Char Int -> Map Char Int
mergeFreq = Map.unionWith (+)

-- Split a list into n nearly equal chunks
splitIntoChunks :: Int -> [a] -> [[a]]
splitIntoChunks n xs
  | n <= 0    = [xs]
  | otherwise = go xs (length xs) n
  where
    go [] _ _ = []
    go ys len k =
      let chunkSize = (len + k - 1) `div` k
          (chunk, rest) = splitAt chunkSize ys
      in chunk : go rest (len - chunkSize) (k - 1)

frequency :: Int -> [Text] -> Map Char Int
frequency nWorkers texts =
  let chunks = splitIntoChunks nWorkers texts
      chunkMaps = map (foldr (mergeFreq . letterFreq) Map.empty) chunks
      -- Parallelism removed due to missing Control.Parallel.Strategies
  in foldr mergeFreq Map.empty chunkMaps
