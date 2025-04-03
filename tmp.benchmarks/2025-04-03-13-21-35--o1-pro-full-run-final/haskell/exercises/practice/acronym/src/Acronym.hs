module Acronym (abbreviate) where

import Data.Char (isAlpha, toUpper, isUpper)

abbreviate :: String -> String
abbreviate input =
  let replacedHyphens   = map (\c -> if c == '-' then ' ' else c) input
      lettersAndSpaces  = filter (\c -> isAlpha c || c == ' ') replacedHyphens
      tokens            = words lettersAndSpaces
      splitByCamelCase  = concatMap splitCamelCase tokens
  in map (toUpper . head) splitByCamelCase

splitCamelCase :: String -> [String]
splitCamelCase []     = []
splitCamelCase (c:cs) = go [c] cs
  where
    go current []     = [current]
    go current (x:xs)
      | isUpper x  = current : go [x] xs
      | otherwise  = go (current ++ [x]) xs
