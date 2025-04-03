module IsbnVerifier (isbn) where

import Data.Char (digitToInt, isDigit)

isbn :: String -> Bool
isbn str = 
    let cleanStr = filter (/= '-') str
    in if isValidFormat cleanStr
       then calculateChecksum cleanStr == 0
       else False

isValidFormat :: String -> Bool
isValidFormat s = 
    length s == 10 && all isValidChar (init s) && isValidLastChar (last s)
  where
    isValidChar c = isDigit c
    isValidLastChar c = isDigit c || c == 'X'

calculateChecksum :: String -> Int
calculateChecksum s = 
    let digits = map charToValue (init s) ++ [lastCharValue (last s)]
    in sum (zipWith (*) digits [10,9..1]) `mod` 11

charToValue :: Char -> Int
charToValue c = digitToInt c

lastCharValue :: Char -> Int
lastCharValue 'X' = 10
lastCharValue c   = digitToInt c
