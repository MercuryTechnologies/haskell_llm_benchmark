module RailFenceCipher (encode, decode) where

import Data.List (sortOn)
import Data.Function (on)

-- Generate the rail pattern indices for a given number of rails and message length
railPattern :: Int -> Int -> [Int]
railPattern rails len = take len $ cyclePattern
  where
    down = [0 .. rails - 1]
    up = [rails - 2, rails - 3 .. 1]
    pattern = down ++ up
    cyclePattern = cycle pattern

encode :: Int -> String -> String
encode rails text =
  concatMap snd sortedGrouped
  where
    patternIndices = railPattern rails (length text)
    zipped = zip patternIndices text
    grouped = [(r, [c | (r', c) <- zipped, r' == r]) | r <- [0 .. rails -1]]
    sortedGrouped = sortOn fst grouped

decode :: Int -> String -> String
decode rails cipherText =
  map snd $ sortOn fst indexedChars
  where
    len = length cipherText
    patternIndices = railPattern rails len
    -- Pair each position with its rail index
    posWithRail = zip [0..] patternIndices
    -- Group positions by rail
    groupedPositions = [(r, [pos | (pos, r') <- posWithRail, r' == r]) | r <- [0 .. rails -1]]
    -- For each rail, take the corresponding number of chars from cipherText
    takeChars _ [] acc = ([], acc)
    takeChars n xs acc = (take n xs, drop n xs ++ acc)
    -- Assign characters to positions
    assignChars [] xs acc = acc
    assignChars ((r, poss):rest) xs acc =
      let (chars, xs') = splitAt (length poss) xs
          newAcc = acc ++ zip poss chars
      in assignChars rest xs' newAcc
    indexedChars = assignChars groupedPositions cipherText []

