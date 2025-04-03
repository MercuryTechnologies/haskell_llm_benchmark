module Hexadecimal (hexToInt) where

hexToInt :: String -> Int
hexToInt s = go s 0
  where
    go [] acc = acc
    go (c:cs) acc =
      case charToDigit c of
        Just d  -> go cs (acc * 16 + d)
        Nothing -> 0

charToDigit :: Char -> Maybe Int
charToDigit '0' = Just 0
charToDigit '1' = Just 1
charToDigit '2' = Just 2
charToDigit '3' = Just 3
charToDigit '4' = Just 4
charToDigit '5' = Just 5
charToDigit '6' = Just 6
charToDigit '7' = Just 7
charToDigit '8' = Just 8
charToDigit '9' = Just 9
charToDigit 'a' = Just 10
charToDigit 'b' = Just 11
charToDigit 'c' = Just 12
charToDigit 'd' = Just 13
charToDigit 'e' = Just 14
charToDigit 'f' = Just 15
charToDigit 'A' = Just 10
charToDigit 'B' = Just 11
charToDigit 'C' = Just 12
charToDigit 'D' = Just 13
charToDigit 'E' = Just 14
charToDigit 'F' = Just 15
charToDigit _   = Nothing
