module Scrabble (scoreLetter, scoreWord) where

-- Define the letter-value mapping
letterValues :: [(Char, Integer)]
letterValues =
  [ ('A', 1), ('E', 1), ('I', 1), ('O', 1), ('U', 1), ('L', 1), ('N', 1), ('R', 1), ('S', 1), ('T', 1)
  , ('D', 2), ('G', 2)
  , ('B', 3), ('C', 3), ('M', 3), ('P', 3)
  , ('F', 4), ('H', 4), ('V', 4), ('W', 4), ('Y', 4)
  , ('K', 5)
  , ('J', 8), ('X', 8)
  , ('Q', 10), ('Z', 10)
  ]

-- Look up a letter's score in the letter-value mapping
scoreLetter :: Char -> Integer
scoreLetter letter = case lookup (toUpper letter) letterValues of
  Just value -> value
  Nothing    -> 0

-- Calculate the Scrabble score of a word
scoreWord :: String -> Integer
scoreWord = sum . map scoreLetter

-- Helper function to convert a character to uppercase
toUpper :: Char -> Char
toUpper c
  | 'a' <= c && c <= 'z' = toEnum (fromEnum c - fromEnum 'a' + fromEnum 'A')
  | otherwise            = c
