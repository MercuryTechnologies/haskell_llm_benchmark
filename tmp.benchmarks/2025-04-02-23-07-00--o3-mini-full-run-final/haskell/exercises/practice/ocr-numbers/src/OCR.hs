module OCR (convert) where

import Data.List (intercalate)
import Data.Maybe (fromMaybe)

digitTemplates :: [([String], Char)]
digitTemplates =
  [ ([" _ ", "| |", "|_|", "   "], '0')
  , (["   ", "  |", "  |", "   "], '1')
  , ([" _ ", " _|", "|_ ", "   "], '2')
  , ([" _ ", " _|", " _|", "   "], '3')
  , (["   ", "|_|", "  |", "   "], '4')
  , ([" _ ", "|_ ", " _|", "   "], '5')
  , ([" _ ", "|_ ", "|_|", "   "], '6')
  , ([" _ ", "  |", "  |", "   "], '7')
  , ([" _ ", "|_|", "|_|", "   "], '8')
  , ([" _ ", "|_|", " _|", "   "], '9')
  ]

recognizeDigit :: [String] -> Char
recognizeDigit ds = fromMaybe '?' $ lookup ds digitTemplates

parseEntry :: [String] -> Either String String
parseEntry linesBlock =
  let rowCount = length linesBlock
      validRowCount = rowCount == 3 || rowCount == 4
  in if not validRowCount
       then Left "error"
       else 
         let fullBlock = if rowCount == 3 then linesBlock ++ ["   "] else linesBlock
             len0 = length (head fullBlock)
         in if any (\row -> length row /= len0) fullBlock || len0 `mod` 3 /= 0
              then Left "error"
              else 
                let numDigits = len0 `div` 3
                    digits = [ recognizeDigit [ take 3 (drop (i * 3) row) | row <- fullBlock ]
                             | i <- [0..numDigits - 1] ]
                in Right digits

splitOnBlank :: [String] -> [[String]]
splitOnBlank [] = []
splitOnBlank xs =
  let (block, rest) = break null xs
      rest' = dropWhile null rest
  in block : splitOnBlank rest'

convert :: String -> String
convert input =
  let allLines = lines input
      blocks = filter (not . null) $ splitOnBlank allLines
      results = map (\block -> case parseEntry block of
                                  Left err -> err
                                  Right res -> res) blocks
  in if any (== "error") results then "error" else intercalate "," results
