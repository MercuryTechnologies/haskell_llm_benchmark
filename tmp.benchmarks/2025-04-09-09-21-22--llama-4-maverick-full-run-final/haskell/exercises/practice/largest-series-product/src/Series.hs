module Series (Error(..), largestProduct) where

import Prelude hiding (product)

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

largestProduct :: Int -> String -> Either Error Integer
largestProduct size digits
  | size < 0 = Left InvalidSpan
  | size > length digits = Left InvalidSpan
  | null digits && size == 0 = Right 1
  | null digits = Left InvalidSpan
  | otherwise = largestProduct' size (map (read . (:[])) digits)

largestProduct' :: Int -> [Integer] -> Either Error Integer
largestProduct' size digits
  | not (all isDigitChar digits) = Left (InvalidDigit (head [c | c <- map (head . show) digits, not (isDigitChar c)]))
  | size == 0 = Right 1
  | otherwise = Right (maximum (map product (take (length digits - size + 1) (tails digits))))

isDigitChar :: Integer -> Bool
isDigitChar x = x >= 0 && x <= 9

product :: [Integer] -> Integer
product = foldl (*) 1

tails :: [a] -> [[a]]
tails xs = xs : case xs of
  [] -> []
  _ : xs' -> tails xs'
