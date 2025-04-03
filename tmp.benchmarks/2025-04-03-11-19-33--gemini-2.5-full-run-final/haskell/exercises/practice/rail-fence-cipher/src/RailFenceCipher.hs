module RailFenceCipher (encode, decode) where

import Data.List (sortOn)

-- Generates the infinite sequence of rail indices in a zig-zag pattern.
-- Example: n=3 -> 0, 1, 2, 1, 0, 1, 2, 1, ...
-- Example: n=4 -> 0, 1, 2, 3, 2, 1, 0, 1, 2, 3, ...
railPattern :: Int -> [Int]
railPattern n
  | n <= 1 = repeat 0 -- For 0 or 1 rail, the index is always 0.
  | otherwise = cycle $ [0..n-1] ++ reverse [1..n-2] -- Generate the pattern 0..n-1 then n-2..1 and cycle it.

-- Encodes a string using the Rail Fence Cipher.
encode :: Int -> String -> String
encode n s
  | n <= 1 = s -- If there's 1 or fewer rails, the message remains unchanged.
  | otherwise =
      let len = length s
          -- Generate the rail index for each character position.
          pattern = take len $ railPattern n
          -- Pair each character with its corresponding rail index.
          charWithRail = zip s pattern
          -- Sort the pairs based on the rail index. Characters on the same rail
          -- maintain their original relative order because sortOn is stable.
          sortedByRail = sortOn snd charWithRail
          -- Extract the characters in the sorted order to form the ciphertext.
      in map fst sortedByRail

-- Decodes a string encoded with the Rail Fence Cipher.
decode :: Int -> String -> String
decode n s
  | n <= 1 = s -- If there's 1 or fewer rails, the message remains unchanged.
  | otherwise =
      let len = length s
          -- Generate the rail index pattern for the length of the ciphertext.
          pattern = take len $ railPattern n
          -- Create pairs of (rail_index, original_position) for each position.
          indices = zip pattern [0..len-1]
          -- Sort these pairs first by rail index, then by original position.
          -- This gives the order of positions corresponding to the ciphertext characters.
          -- Example: For n=3, s="WECRLTE...", sorted_indices starts [(0,0), (0,4), (0,8), ..., (1,1), (1,3), ...]
          sorted_indices = sortOn id indices
          -- Pair the characters of the ciphertext with their corresponding (rail, original_position) info.
          -- Example: [('W', (0,0)), ('E', (0,4)), ..., ('E', (1,1)), ('R', (1,3)), ...]
          char_with_indices = zip s sorted_indices
          -- Sort these pairs based on the original position to restore the original order.
          -- Example: [('W', (0,0)), ('E', (1,1)), ('A', (2,2)), ('R', (1,3)), ...]
          sorted_by_orig_idx = sortOn (snd . snd) char_with_indices
          -- Extract the characters in the restored original order.
      in map fst sorted_by_orig_idx
