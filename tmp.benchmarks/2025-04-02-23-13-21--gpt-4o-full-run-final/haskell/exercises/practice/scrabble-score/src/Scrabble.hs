module Scrabble (scoreLetter, scoreWord) where

import Data.Char (toUpper)

scoreLetter :: Char -> Integer
scoreLetter letter
  | upperLetter `elem` "AEIOULNRST" = 1
  | upperLetter `elem` "DG"         = 2
  | upperLetter `elem` "BCMP"       = 3
  | upperLetter `elem` "FHVWY"      = 4
  | upperLetter == 'K'              = 5
  | upperLetter `elem` "JX"         = 8
  | upperLetter `elem` "QZ"         = 10
  | otherwise                       = 0
  where upperLetter = toUpper letter

scoreWord :: String -> Integer
scoreWord = sum . map scoreLetter
