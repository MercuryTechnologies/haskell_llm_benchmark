module IsbnVerifier (isbn) where

import Data.Char (isDigit, digitToInt)

isbn :: String -> Bool
isbn str =
  let cleaned = filter (/= '-') str
  in length cleaned == 10 &&
     all isDigit (init cleaned) &&
     (isDigit (last cleaned) || last cleaned == 'X') &&
     (sum (zipWith (*) (map charToValue cleaned) [10, 9..1])) `mod` 11 == 0

charToValue :: Char -> Int
charToValue 'X' = 10
charToValue c   = digitToInt c
