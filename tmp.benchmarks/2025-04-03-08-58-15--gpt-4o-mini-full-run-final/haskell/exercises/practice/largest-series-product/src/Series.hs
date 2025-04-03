module Series (Error(..), largestProduct) where

import Data.Char (digitToInt, isDigit)

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

largestProduct :: Int -> String -> Either Error Integer
largestProduct size digits
    | size < 0 = Left InvalidSpan
    | size == 0 = Right 1
    | size > length digits = Left InvalidSpan
    | any (not . isDigit) digits = Left (InvalidDigit ' ')
    | otherwise = Right $ maximum $ map productOfSeries series
  where
    series = [take size (drop i digits) | i <- [0..length digits - size]]
    productOfSeries s = product $ map digitToInt s
