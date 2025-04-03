module Diamond (diamond) where

import Data.Char (chr, ord)

diamond :: Char -> Maybe [String]
diamond c
  | c < 'A' || c > 'Z' = Nothing
  | otherwise = Just (topHalf ++ bottomHalf)
  where
    n = ord c - ord 'A'
    topHalf    = [mkLine i | i <- [0..n]]
    bottomHalf = [mkLine i | i <- [n-1,n-2..0]]

    mkLine i =
      let letter       = chr (ord 'A' + i)
          leadingSpaces = replicate (n - i) ' '
          innerSpaces   = replicate (2 * i - 1) ' '
      in if i == 0
         then leadingSpaces ++ [letter] ++ leadingSpaces
         else leadingSpaces ++ [letter] ++ innerSpaces ++ [letter] ++ leadingSpaces
