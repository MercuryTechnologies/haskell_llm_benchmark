module OCR (convert) where

import Data.List (zip, lookup, any, elem, length, lines, dropWhile, null)

-- Patterns for digits 0 through 9, each digit is 4 lines of 3 characters
digitMap :: [[String]]
digitMap =
  [ [ " _ "
    , "| |"
    , "|_|"
    , "   "
    ] -- '0'
  , [ "   "
    , "  |"
    , "  |"
    , "   "
    ] -- '1'
  , [ " _ "
    , " _|"
    , "|_ "
    , "   "
    ] -- '2'
  , [ " _ "
    , " _|"
    , " _|"
    , "   "
    ] -- '3'
  , [ "   "
    , "|_|"
    , "  |"
    , "   "
    ] -- '4'
  , [ " _ "
    , "|_ "
    , " _|"
    , "   "
    ] -- '5'
  , [ " _ "
    , "|_ "
    , "|_|"
    , "   "
    ] -- '6'
  , [ " _ "
    , "  |"
    , "  |"
    , "   "
    ] -- '7'
  , [ " _ "
    , "|_|"
    , "|_|"
    , "   "
    ] -- '8'
  , [ " _ "
    , "|_|"
    , " _|"
    , "   "
    ] -- '9'
  ]

-- For convenience, pair the above patterns with their corresponding digit chars
digitPairs :: [([String], Char)]
digitPairs = zip digitMap (map (head . show) [0..9])

-- Given 4 lines of length-3 strings, look up the digit
recognizeDigit :: [String] -> Char
recognizeDigit chunk =
  case lookup chunk digitPairs of
    Just c  -> c
    Nothing -> '?'

-- Split all lines into blocks of 4 lines each, separated by blank lines
splitBlocks :: [String] -> [[String]]
splitBlocks strs =
  let skipBlanks = dropWhile null strs
  in case skipBlanks of
       [] -> []
       _  ->
         let (block, rest) = splitAt 4 skipBlanks
         in if length block < 4
              then error "Invalid input size"
              else block : splitBlocks (dropWhile null rest)

-- Convert a single block (4 lines) into the recognized digit-string
convertBlock :: [String] -> String
convertBlock fourLines =
  let w = length (head fourLines)
  in
    if any (\ln -> length ln /= w) fourLines
      then error "Invalid input size"
      else if w `mod` 3 /= 0
        then error "Invalid input size"
        else
          let digitsCount = w `div` 3
              -- For each digit in this block, slice out columns
              oneDigit i = map (take 3 . drop (3*i)) fourLines
              recognized = map (recognizeDigit . oneDigit) [0..digitsCount-1]
          in recognized

-- Main entry: convert the entire input into a string
convert :: String -> String
convert input =
  let allLines = lines input
      blocks = splitBlocks allLines
      results = map convertBlock blocks
  in case results of
       []    -> ""
       (_:_) -> concatWithCommas results

-- Join multiple digit-strings with commas
concatWithCommas :: [String] -> String
concatWithCommas = foldr1 (\a b -> a ++ "," ++ b)
