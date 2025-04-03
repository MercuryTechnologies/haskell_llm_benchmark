module Series (Error(..), largestProduct) where

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

largestProduct :: Int -> String -> Either Error Integer
largestProduct size digits
  | size < 0 = Left InvalidSpan
  | size > length digits = Left InvalidSpan
  | size == 0 = Right 1  -- Product of empty sequence is 1 (multiplicative identity)
  | otherwise = case validateDigits digits of
      Left err -> Left err
      Right validDigits -> Right $ findLargestProduct size validDigits

-- Validate that all characters in the input are digits
validateDigits :: String -> Either Error [Integer]
validateDigits = traverse charToDigit
  where
    charToDigit :: Char -> Either Error Integer
    charToDigit c
      | c >= '0' && c <= '9' = Right $ toInteger (fromEnum c - fromEnum '0')
      | otherwise = Left $ InvalidDigit c

-- Find the largest product of consecutive digits
findLargestProduct :: Int -> [Integer] -> Integer
findLargestProduct size digits
  | length digits < size = 0  -- This shouldn't happen due to earlier checks
  | otherwise = maximum $ map product $ slidingWindow size digits

-- Create sliding windows of specified size from a list
slidingWindow :: Int -> [a] -> [[a]]
slidingWindow size xs
  | length xs < size = []
  | otherwise = take size xs : slidingWindow size (tail xs)
