module Diamond (diamond) where

import Data.Char (chr, ord)

diamond :: Char -> Maybe [String]
diamond c
  | c < 'A' || c > 'Z' = Nothing
  | otherwise = Just $ top ++ tail (reverse top)
  where
    n = ord c - ord 'A'
    makeRow i
      | i == 0    = replicate (n - i) ' ' ++ [chr (ord 'A' + i)] ++ replicate (n - i) ' '
      | otherwise = replicate (n - i) ' ' 
                    ++ [chr (ord 'A' + i)] 
                    ++ replicate (2 * i - 1) ' ' 
                    ++ [chr (ord 'A' + i)] 
                    ++ replicate (n - i) ' '
    top = [makeRow i | i <- [0 .. n]]
