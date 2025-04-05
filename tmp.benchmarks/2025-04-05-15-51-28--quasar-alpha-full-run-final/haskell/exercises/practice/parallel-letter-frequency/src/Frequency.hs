module Frequency (frequency) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Control.Parallel.Strategies (parListChunk, rdeepseq, withStrategy)

frequency :: Int -> [Text] -> Map Char Int
frequency nWorkers texts =
    let chunks = chunkList nWorkers texts
        partials = withStrategy (parListChunk 1 rdeepseq) $ map countChunk chunks
    in foldl (Map.unionWith (+)) Map.empty partials

countChunk :: [Text] -> Map Char Int
countChunk = foldl (Map.unionWith (+)) Map.empty . map countText

countText :: Text -> Map Char Int
countText = T.foldl' update Map.empty
  where
    update acc c
      | isLetter c = Map.insertWith (+) c 1 acc
      | otherwise  = acc

isLetter :: Char -> Bool
isLetter c = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')

chunkList :: Int -> [a] -> [[a]]
chunkList n xs
  | n <= 0    = [xs]
  | otherwise = go xs (max 1 (length xs `div` n))
  where
    go [] _ = []
    go ys size = let (chunk, rest) = splitAt size ys
                 in chunk : go rest size
