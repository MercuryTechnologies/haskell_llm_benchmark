module Series (Error(..), largestProduct) where

import Data.Char (isDigit, digitToInt)

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

largestProduct :: Int -> String -> Either Error Integer
largestProduct size digits
  | size < 0               = Left InvalidSpan
  | size > length digits   = Left InvalidSpan
  | otherwise =
      case filter (not . isDigit) digits of
        (c:_) -> Left (InvalidDigit c)
        []    | size == 0  -> Right 1
              | otherwise  ->
                  let blocks = consecutiveBlocks size digits
                      productOfBlock block = product (map (toInteger . digitToInt) block)
                  in Right (maximum (map productOfBlock blocks))

consecutiveBlocks :: Int -> [a] -> [[a]]
consecutiveBlocks n xs
  | n > length xs = []
  | otherwise     = take n xs : consecutiveBlocks n (tail xs)
