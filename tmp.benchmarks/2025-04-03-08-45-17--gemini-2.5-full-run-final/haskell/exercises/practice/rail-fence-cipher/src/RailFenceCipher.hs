module RailFenceCipher (encode, decode) where

import Data.List (sortOn) -- Use sortOn for stable sorting by key

-- Generates the infinite sequence of rail indices (0-based)
-- e.g., n=3 -> 0,1,2,1,0,1,2,1,...
-- e.g., n=4 -> 0,1,2,3,2,1,0,1,2,3,...
-- e.g., n=1 -> 0,0,0,...
railIndices :: Int -> [Int]
railIndices n
  | n <= 0 = error "Number of rails must be positive" -- As per original stub, error on invalid input
  | n == 1 = repeat 0
  | otherwise = cycle $ [0 .. n - 1] ++ [n - 2, n - 3 .. 1]

-- | Encodes a message using the Rail Fence Cipher.
encode :: Int -> String -> String
encode n msg
  | n <= 0 = error "Number of rails must be positive"
  | n == 1 = msg
  | null msg = ""
  | otherwise =
      -- Pair chars with their rail index: (rail_index, char)
      let railCharPairs = zip (take (length msg) $ railIndices n) msg
      -- Sort by rail index. sortOn is stable, preserving original order within a rail.
          sortedByRail = sortOn fst railCharPairs
      -- Extract characters in the new order
      in map snd sortedByRail

-- | Decodes a message encoded using the Rail Fence Cipher.
decode :: Int -> String -> String
decode n encodedMsg
  | n <= 0 = error "Number of rails must be positive"
  | n == 1 = encodedMsg
  | null encodedMsg = ""
  | otherwise =
      let len = length encodedMsg
          -- Generate the rail pattern for the message length: (original_index, rail_index)
          indexedRails = zip [0..] (take len $ railIndices n)
          -- Sort by rail index to determine the order characters appear in the encoded string.
          -- Result: [(original_index, rail_index)] sorted by rail_index
          sortedIndexedRails = sortOn snd indexedRails
          -- Pair the characters from the encoded string with their original positions/rails
          -- Result: ((original_index, rail_index), char)
          charsWithOrigPos = zip sortedIndexedRails encodedMsg
          -- Sort back to the original message order using original_index
          -- Result: ((original_index, rail_index), char) sorted by original_index
          sortedBackToOrig = sortOn (fst . fst) charsWithOrigPos
      -- Extract the characters in their original order
      in map snd sortedBackToOrig
