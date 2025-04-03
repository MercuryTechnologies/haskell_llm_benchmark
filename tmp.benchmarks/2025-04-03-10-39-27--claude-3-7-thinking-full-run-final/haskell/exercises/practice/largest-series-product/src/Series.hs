module Series (Error(..), largestProduct) where

import Data.Char (isDigit, digitToInt)

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

largestProduct :: Int -> String -> Either Error Integer
largestProduct size digits
  | size < 0 = Left InvalidSpan
  | size == 0 = Right 1
  | size > length digits = Left InvalidSpan
  | not (all isDigit digits) = Left $ InvalidDigit (head $ filter (not . isDigit) digits)
  | otherwise = Right $ maximum $ map (product . map (toInteger . digitToInt)) $ windows size digits
  where
    windows :: Int -> [a] -> [[a]]
    windows n xs
      | length xs < n = []
      | otherwise = take n xs : windows n (tail xs)
