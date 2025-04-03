module RailFenceCipher (encode, decode) where

import Data.List (sortOn)

zigzagIndices :: Int -> Int -> [Int]
zigzagIndices r n
  | r <= 1 = replicate n 0
  | otherwise = take n $ cycle ( [0..r-1] ++ [r-2, r-3..1] )

encode :: Int -> String -> String
encode r msg =
  let filtered     = filter (/= ' ') msg
      pattern      = zigzagIndices r (length filtered)
      paired       = zip pattern filtered
  in concat [ [ c | (railIdx, c) <- paired, railIdx == rail ] | rail <- [0..r-1] ]

decode :: Int -> String -> String
decode r encrypted =
  let n            = length encrypted
      pattern      = zigzagIndices r n
      posAndRail   = zip [0..] pattern
      sortedByRail = sortOn snd posAndRail
      assigned     = zip sortedByRail encrypted
      sortedByPos  = sortOn (fst . fst) assigned
  in map snd sortedByPos
