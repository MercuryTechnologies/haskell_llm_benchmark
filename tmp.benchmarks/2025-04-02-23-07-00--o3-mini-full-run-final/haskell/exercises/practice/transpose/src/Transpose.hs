module Transpose (transpose) where

-- | Transpose a list of strings.
-- For rows of different lengths, we align the rows to the left.
-- That is, a missing character at a given index is treated as a pad character (' ').
-- When transposing, pad characters at the end of an output row (i.e. trailing pad characters)
-- are removed, but a space that was originally present is never removed.
transpose :: [String] -> [String]
transpose [] = []
transpose rows =
  let maxLen = maximum (map length rows)
      -- For a given row and index i, if the row has a character at i, return it (with flag False),
      -- otherwise return a pad space (with flag True).
      getChar row i = if i < length row then (row !! i, False) else (' ', True)
      -- Build the transposed matrix as a list of rows, where each row is a list of (Char,Bool)
      -- The Bool flag indicates whether the character is a pad-space.
      transposedWithFlags = [ [ getChar row i | row <- rows ] | i <- [0 .. maxLen - 1] ]
      -- For each row, remove trailing pad spaces.
      trimPad xs = let xs' = reverse (dropWhile (\(_, isPad) -> isPad) (reverse xs))
                   in map fst xs'
  in map trimPad transposedWithFlags
