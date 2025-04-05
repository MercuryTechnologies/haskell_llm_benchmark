module Series (Error(..), largestProduct) where

import Data.Char (isDigit, digitToInt)
import Data.List (tails)

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

largestProduct :: Int -> String -> Either Error Integer
largestProduct size digits
  | size < 0 = Left InvalidSpan
  | size > length digits = Left InvalidSpan
  | otherwise = case checkDigits digits of
      Just err -> Left err
      Nothing ->
        if size == 0
          then Right 1
          else Right $ maximum $ map productOfSlice (slices size digits)
  where
    checkDigits :: String -> Maybe Error
    checkDigits [] = Nothing
    checkDigits (c:cs)
      | isDigit c = checkDigits cs
      | otherwise = Just (InvalidDigit c)

    slices :: Int -> String -> [String]
    slices n xs = [take n t | t <- tails xs, length t >= n]

    productOfSlice :: String -> Integer
    productOfSlice = product . map (toInteger . digitToInt)
