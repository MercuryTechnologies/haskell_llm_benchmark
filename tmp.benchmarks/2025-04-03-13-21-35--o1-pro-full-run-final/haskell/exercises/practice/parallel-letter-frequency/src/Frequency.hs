module Frequency (frequency) where

import Data.Char (toLower, isAlpha)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Control.Parallel (par, pseq)

-- | Count the frequency of letters in the given list of texts,
-- using the specified number of workers to process them in parallel.
frequency :: Int -> [Text] -> Map Char Int
frequency nWorkers texts =
  let parts = chunkList nWorkers texts
      partialMaps = map freq parts
  in foldr combine M.empty partialMaps

-- | Count frequencies in a chunk of texts.
freq :: [Text] -> Map Char Int
freq = foldr (M.unionWith (+)) M.empty . map freqOne

-- | Count frequencies in a single Text.
freqOne :: Text -> Map Char Int
freqOne = T.foldr insertChar M.empty
  where
    insertChar c acc
      | isAlpha c = M.insertWith (+) (toLower c) 1 acc
      | otherwise = acc

-- | Combine two Maps in parallel, forcing each side before unioning.
combine :: Map Char Int -> Map Char Int -> Map Char Int
combine m1 m2 =
  let m1' = forceMap m1
      m2' = forceMap m2
  in m1' `par` (m2' `pseq` M.unionWith (+) m1' m2')

-- | Force evaluation of the map to help parallel performance.
forceMap :: Map Char Int -> Map Char Int
forceMap m = M.foldr (+) 0 m `seq` m

-- | Chunk the list into N sublists; each sublist is processed by one worker.
chunkList :: Int -> [a] -> [[a]]
chunkList _ [] = []
chunkList n xs
  | n <= 1 = [xs]
  | otherwise =
      let len = length xs
          size = (len + n - 1) `div` n
      in chunk size xs
  where
    chunk _ [] = []
    chunk k ys =
      let (firstPart, rest) = splitAt k ys
      in firstPart : chunk k rest
