module Series (Error(..), largestProduct) where

import Data.Char (isDigit, digitToInt)
import Data.List (tails)

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

largestProduct :: Int -> String -> Either Error Integer
largestProduct size digits
  | size < 0 = Left InvalidSpan                   -- Span cannot be negative
  | size == 0 = Right 1                            -- Product of empty series is 1
  | size > length digits = Left InvalidSpan        -- Span cannot be longer than the input string
  | otherwise = do
      -- Validate all characters are digits and convert them to Integers.
      -- `traverse` applies `charToDigit` to each char and collects results in Either.
      -- If any charToDigit returns Left, the whole result is Left.
      intList <- traverse charToDigit digits

      -- Generate all sublists (series) of the specified size.
      -- `tails` generates all suffixes: e.g., "123" -> ["123", "23", "3", ""]
      -- `map (take size)` takes the first 'size' elements from each suffix.
      -- `filter ((== size) . length)` keeps only those lists that actually have the required size.
      let seriesList = map (take size) (tails intList)
          validSeries = filter ((== size) . length) seriesList

      -- Calculate the product for each valid series and find the maximum.
      -- Note: `validSeries` cannot be empty here due to the initial checks
      -- (size > 0 and size <= length digits implies length digits > 0).
      Right $ maximum $ map product validSeries

-- Helper function to convert a single character to an Integer,
-- returning an error if it's not a digit.
charToDigit :: Char -> Either Error Integer
charToDigit c
  | isDigit c = Right $ fromIntegral $ digitToInt c -- Convert Char digit to Integer
  | otherwise = Left $ InvalidDigit c              -- Return error for non-digits
