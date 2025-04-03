module IsbnVerifier (isbn) where

import Data.Char (digitToInt, isDigit)

isbn :: String -> Bool
isbn input =
  let noDashes = filter (/= '-') input
  in if length noDashes /= 10 then False
     else if not (allValidChars noDashes) then False
          else let weights = [10,9..1]
                   values  = map charValue noDashes
                   total   = sum $ zipWith (*) values weights
               in total `mod` 11 == 0

allValidChars :: String -> Bool
allValidChars xs = all validAtIndex (zip xs [0..])
  where
    validAtIndex (c, idx)
      | idx < 9   = isDigit c
      | otherwise = isDigit c || c == 'X'

charValue :: Char -> Int
charValue 'X' = 10
charValue c   = digitToInt c
