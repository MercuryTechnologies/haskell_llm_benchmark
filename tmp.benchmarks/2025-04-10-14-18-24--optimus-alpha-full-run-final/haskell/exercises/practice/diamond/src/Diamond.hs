module Diamond (diamond) where

import Data.Char (ord, chr)

diamond :: Char -> Maybe [String]
diamond c
  | not (isValid c) = Nothing
  | otherwise       = Just $ map (row n) [0..(2 * n - 2)]
  where
    isValid ch = ch >= 'A' && ch <= 'Z'
    n = ord c - ord 'A' + 1
    row totalRows i =
      let
        -- Calculate which letter this row is (0-based: 0 = 'A', ..., n-1 = c)
        letterIdx = if i < n then i else 2 * n - 2 - i
        letter = chr (ord 'A' + letterIdx)
        -- Number of leading/trailing spaces
        outerSpaces = abs (n - 1 - i)
        -- Number of spaces between the two letters (for 'A' it's 0)
        innerSpaces = if letterIdx == 0 then 0 else 2 * letterIdx - 1
        -- Build the row
        left = replicate outerSpaces ' '
        right = replicate outerSpaces ' '
        middle
          | letterIdx == 0 = [letter]
          | otherwise      = letter : replicate innerSpaces ' ' ++ [letter]
      in left ++ middle ++ right
