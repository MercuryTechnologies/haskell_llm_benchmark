module Acronym (abbreviate) where

import Data.Char (toUpper, isAlpha, isSpace, isUpper)

abbreviate :: String -> String
abbreviate input =
    let replaced = map (\c -> if c == '-' then ' ' else c) input
        cleaned  = filter (\c -> isAlpha c || isSpace c) replaced
        wds      = words cleaned
    in map (toUpper . head) (concatMap splitCamelCase wds)

splitCamelCase :: String -> [String]
splitCamelCase [] = []
splitCamelCase (x:xs) = go [x] xs
  where
    go acc [] = [acc]
    go acc (c:cs)
      | isUpper c = acc : go [c] cs
      | otherwise = go (acc ++ [c]) cs
