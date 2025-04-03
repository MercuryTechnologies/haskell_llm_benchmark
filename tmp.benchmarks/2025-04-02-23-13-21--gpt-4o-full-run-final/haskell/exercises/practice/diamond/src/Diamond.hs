module Diamond (diamond) where

import Data.Maybe (fromMaybe)
import Data.Char (ord, chr)

diamond :: Char -> Maybe [String]
diamond c
  | c < 'A' || c > 'Z' = Nothing
  | otherwise = Just $ topHalf ++ bottomHalf
  where
    n = ord c - ord 'A'
    topHalf = [row i | i <- [0..n]]
    bottomHalf = reverse (init topHalf)
    row i = replicate (n - i) ' ' ++ [chr (ord 'A' + i)] ++ replicate (2 * i - 1) ' ' ++ [chr (ord 'A' + i) | i > 0] ++ replicate (n - i) ' '
