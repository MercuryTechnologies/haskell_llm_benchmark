module Series (Error(..), largestProduct) where

import Data.Char (isDigit, digitToInt)
import Data.List (tails)

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

largestProduct :: Int -> String -> Either Error Integer
largestProduct span digits
  | span < 0 || span > length digits = Left InvalidSpan
  | otherwise =
      case findInvalid digits of
        Just c  -> Left (InvalidDigit c)
        Nothing -> Right (if span == 0 then 1 else maximum products)
  where
    nums = map (toInteger . digitToInt) digits
    windows = [ take span xs | xs <- tails nums, length xs >= span ]
    products = map product windows

findInvalid :: String -> Maybe Char
findInvalid [] = Nothing
findInvalid (c:cs)
  | not (isDigit c) = Just c
  | otherwise       = findInvalid cs
