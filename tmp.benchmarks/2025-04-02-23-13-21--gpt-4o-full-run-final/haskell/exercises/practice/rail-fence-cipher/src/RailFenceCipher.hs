module RailFenceCipher (encode, decode) where

import Data.List (sortOn)
import Data.Function (on)

-- Helper function to generate the zig-zag pattern indices
zigzagIndices :: Int -> Int -> [Int]
zigzagIndices rails len = take len $ cycle (down ++ up)
  where
    down = [0..rails-1]
    up = [rails-2,rails-3..1]

-- Encode function
encode :: Int -> String -> String
encode rails text = concat $ map snd $ sortOn fst $ zip (zigzagIndices rails (length text)) (map (:[]) text)

-- Decode function
decode :: Int -> String -> String
decode rails text = map snd $ sortOn fst $ zip sortedIndices text
  where
    len = length text
    indices = zigzagIndices rails len
    sortedIndices = sortOn snd $ zip indices [0..]
