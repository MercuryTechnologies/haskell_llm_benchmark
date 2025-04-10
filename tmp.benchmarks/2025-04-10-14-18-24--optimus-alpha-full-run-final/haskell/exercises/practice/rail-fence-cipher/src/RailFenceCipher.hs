module RailFenceCipher (encode, decode) where

import Data.List (sortOn)

-- Helper: generate the rail pattern for a given number of rails and message length
railPattern :: Int -> Int -> [Int]
railPattern rails len
  | rails <= 1 = replicate len 0
  | otherwise  = take len $ cycle (down ++ up)
  where
    down = [0 .. rails - 1]
    up   = [rails - 2, rails - 3 .. 1]

-- ENCODE: Write the message in zig-zag, then read off by rails
encode :: Int -> String -> String
encode rails msg =
  let pat = railPattern rails (length msg)
      zipped = zip pat msg
      sorted = sortOn fst zipped
  in map snd sorted

-- DECODE: Reconstruct the zig-zag, then read off in original order
decode :: Int -> String -> String
decode rails cipher =
  let len = length cipher
      pat = railPattern rails len
      -- For each rail, count how many chars go there
      railCounts = [length $ filter (== r) pat | r <- [0 .. rails - 1]]
      -- For each rail, take the corresponding chars from cipher
      railSlices = snd $ foldl (\(i, acc) c -> (i + c, acc ++ [take c $ drop i cipher])) (0, []) railCounts
      -- For each position, which rail does it belong to?
      railPositions = pat
      -- For each rail, keep a pointer to the next char to use
      build [] _ _ = []
      build (r:xs) railsList ptrs =
        let c = (railsList !! r) !! (ptrs !! r)
            ptrs' = take r ptrs ++ [ptrs !! r + 1] ++ drop (r + 1) ptrs
        in c : build xs railsList ptrs'
  in build railPositions railSlices (replicate rails 0)
