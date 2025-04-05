module Frequency (frequency) where

import Data.Map  (Map, empty, unionWith, fromListWith)
import Data.Text (Text, unpack, toLower)
import Control.Parallel.Strategies (parListChunk, rseq)

frequency :: Int -> [Text] -> Map Char Int
frequency nWorkers texts = 
  let chunkSize = max 1 (length texts `div` nWorkers)
      chunks = chunk texts chunkSize
      freqs = map (countFrequency . concatMap (unpack . toLower)) chunks `using` parListChunk chunkSize rseq
  in foldl (unionWith (+)) empty freqs

countFrequency :: String -> Map Char Int
countFrequency = fromListWith (+) . map (\c -> (c, 1))

chunk :: [a] -> Int -> [[a]]
chunk xs n = chunk' xs n []
  where
    chunk' :: [a] -> Int -> [[a]] -> [[a]]
    chunk' [] _ acc = reverse acc
    chunk' xs n acc = chunk' (drop n xs) n ((take n xs) : acc)
