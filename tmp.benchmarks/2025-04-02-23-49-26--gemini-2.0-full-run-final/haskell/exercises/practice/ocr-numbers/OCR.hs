module OCR (convert) where

import Data.List (intercalate)

-- Define the digit representations
digitRepresentations :: [(String, Char)]
digitRepresentations =
  [ ( " _ " ++
      "| |" ++
      "|_|",
      '0'
    ),
    ( "   " ++
      "  |" ++
      "  |",
      '1'
    ),
    ( " _ " ++
      " _|" ++
      "|_ ",
      '2'
    ),
    ( " _ " ++
      " _|" ++
      " _|",
      '3'
    ),
    ( "   " ++
      "|_|" ++
      "  |",
      '4'
    ),
    ( " _ " ++
      "|_ " ++
      " _|",
      '5'
    ),
    ( " _ " ++
      "|_ " ++
      "|_|",
      '6'
    ),
    ( " _ " ++
      "  |" ++
      "  |",
      '7'
    ),
    ( " _ " ++
      "|_|" ++
      "|_|",
      '8'
    ),
    ( " _ " ++
      "|_|" ++
      " _|",
      '9'
    )
  ]

-- Function to convert a single 3x4 block to a digit
recognizeDigit :: [String] -> Char
recognizeDigit inputLines =
  let block = concat inputLines
      match = lookup block digitRepresentations
   in case match of
        Just digit -> digit
        Nothing -> '?'

-- Function to convert a line of OCR numbers
convertLine :: String -> String
convertLine line =
  let chunks = chunkString 3 line
      -- Check if the line has a valid number of characters
      isValidLine = all (\s -> length s == 3) chunks
      -- Split the line into 3-character chunks
      chunkString :: Int -> String -> [String]
      chunkString _ [] = []
      chunkString n str = take n str : chunkString n (drop n str)
      -- Group the chunks into sets of 3 lines
      groupedChunks = groupIntoThrees $ lines line
      groupIntoThrees :: [String] -> [[String]]
      groupIntoThrees [] = []
      groupIntoThrees lines
        | length lines >= 3 = take 3 lines : groupIntoThrees (drop 3 lines)
        | otherwise = []
      -- Recognize each digit in the line
      recognizedDigits = map recognizeDigit groupedChunks
   in if not isValidLine
        then error "Invalid input size"
        else recognizedDigits

-- Main convert function
convert :: String -> String
convert input =
  let linesOfNumbers = lines input
   in intercalate "," (map convertLine linesOfNumbers)
