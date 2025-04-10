module Transpose (transpose) where

import qualified Data.List as L

-- Pad each string on the left with spaces to the length of the longest string
padLeft :: Int -> String -> String
padLeft n s = replicate (n - length s) ' ' ++ s

transpose :: [String] -> [String]
transpose xs =
  let maxLen = maximum (0 : map length xs)
      padded = map (padLeft maxLen) xs
      transposed = L.transpose padded
      -- Remove trailing spaces from each row (but not leading)
      rstrip = reverse . dropWhile (== ' ') . reverse
  in map rstrip transposed
