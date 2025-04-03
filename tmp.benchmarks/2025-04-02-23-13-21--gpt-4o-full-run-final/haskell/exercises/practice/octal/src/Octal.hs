module Octal (readOct, showOct) where

import Data.Char (digitToInt, isDigit)

readOct :: String -> Integer
readOct s
  | all isValidOctalDigit s = foldl (\acc x -> acc * 8 + toInteger (digitToInt x)) 0 s
  | otherwise = 0
  where
    isValidOctalDigit c = isDigit c && c < '8'

showOct :: Integer -> String
showOct = error "You need to implement this function."
