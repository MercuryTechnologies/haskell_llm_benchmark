module RailFenceCipher (encode, decode) where

import Data.List (sortOn)

-- Helper to generate the rail pattern for each character's position.
-- For rails > 1, we use the zig-zag pattern: 
--   Example for rails = 3: [0,1,2,1,0,1,2,1,0,...]
railPattern :: Int -> Int -> [Int]
railPattern rails len
  | rails <= 1 = replicate len 0
  | otherwise = take len $ cycle (asc ++ desc)
  where
    asc = [0 .. rails - 1]
    desc = [rails - 2, rails - 3 .. 1]

-- Encoding: put each character into its rail based on the zig-zag pattern,
-- then read the rails row by row.
encode :: Int -> String -> String
encode rails text
  | rails <= 1 = text
  | otherwise = concat [ [ c | (c, r) <- zip text pattern, r == rail ] | rail <- [0..rails-1] ]
  where
    pattern = railPattern rails (length text)

-- Helper: splits a string into pieces with the given lengths.
splitByCounts :: [Int] -> String -> [String]
splitByCounts [] _ = []
splitByCounts (n:ns) s =
  let (front, rest) = splitAt n s
  in front : splitByCounts ns rest

-- Decoding:
-- 1. Rebuild the zig-zag pattern (list of rail indices for each character position).
-- 2. For each rail, determine the positions in the original string.
-- 3. Split the ciphertext into segments corresponding to each rail.
-- 4. Assign each segment's characters back to their original positions.
decode :: Int -> String -> String
decode rails cipherText
  | rails <= 1 = cipherText
  | otherwise = map snd $ sortOn fst assignments
  where
    len = length cipherText
    pattern = railPattern rails len
    -- For each rail, get all positions where that rail occurs.
    railPositions = [ [ i | (i, r) <- zip [0..] pattern, r == rail ] | rail <- [0..rails-1] ]
    -- Counts for each rail, i.e. how many characters go in each.
    counts = map length railPositions
    -- Split ciphertext into segments based on the counts.
    segments = splitByCounts counts cipherText
    -- Combine positions with corresponding segment characters.
    assignments = concat [ zip positions seg | (positions, seg) <- zip railPositions segments ]
