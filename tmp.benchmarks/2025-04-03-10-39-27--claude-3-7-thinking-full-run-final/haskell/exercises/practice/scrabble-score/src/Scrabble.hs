module Scrabble (scoreLetter, scoreWord) where

import Data.Char (toUpper)

scoreLetter :: Char -> Integer
scoreLetter letter =
  case toUpper letter of
    c | c `elem` "AEIOULNRST" -> 1
      | c `elem` "DG" -> 2
      | c `elem` "BCMP" -> 3
      | c `elem` "FHVWY" -> 4
      | c == 'K' -> 5
      | c `elem` "JX" -> 8
      | c `elem` "QZ" -> 10
      | otherwise -> 0  -- Non-letter characters score 0

scoreWord :: String -> Integer
scoreWord word = sum (map scoreLetter word)
