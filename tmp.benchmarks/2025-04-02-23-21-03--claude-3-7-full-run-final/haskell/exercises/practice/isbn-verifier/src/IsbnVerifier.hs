module IsbnVerifier (isbn) where

import Data.Char (isDigit, digitToInt)

isbn :: String -> Bool
isbn xs
  | length cleanInput /= 10 = False
  | not (all isValidChar cleanInput) = False
  | otherwise = checksum cleanInput `mod` 11 == 0
  where
    cleanInput = filter (/= '-') xs
    
    isValidChar c = isDigit c || (c == 'X' && c == last cleanInput)
    
    charToValue c
      | isDigit c = digitToInt c
      | c == 'X' = 10
      | otherwise = 0  -- This case should never happen due to isValidChar check
    
    checksum s = sum $ zipWith (*) (map charToValue s) [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]
