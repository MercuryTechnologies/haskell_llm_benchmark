module Scrabble (scoreLetter, scoreWord) where

import Data.Char (toUpper)

scoreLetter :: Char -> Integer
scoreLetter letter
  | up `elem` "AEIOULNRST" = 1
  | up `elem` "DG"         = 2
  | up `elem` "BCMP"       = 3
  | up `elem` "FHVWY"      = 4
  | up == 'K'              = 5
  | up `elem` "JX"         = 8
  | up `elem` "QZ"         = 10
  | otherwise              = 0
  where
    up = toUpper letter

scoreWord :: String -> Integer
scoreWord word = sum (map scoreLetter word)
