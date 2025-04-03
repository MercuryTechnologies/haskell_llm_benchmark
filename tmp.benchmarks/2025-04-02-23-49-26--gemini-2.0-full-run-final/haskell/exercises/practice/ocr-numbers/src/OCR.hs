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
convertLine :: [String] -> String
convertLine lines =
  let
      -- Check if the line has a valid number of characters
      isValidLine = all (\line -> length line `mod` 3 == 0) lines
      -- Split the line into 3-character chunks
      chunkString :: Int -> String -> [String]
      chunkString _ [] = []
      chunkString n str = take n str : chunkString n (drop n str)
      -- Group the lines into sets of 4, and then chunk each of the 4 lines into groups of 3
      groupedChunks = map (chunkString 3) lines
      -- Transpose the grouped chunks so that we have a list of 4-line chunks
      transposeChunks :: [[String]] -> [[String]]
      transposeChunks [] = []
      transposeChunks ([]:_) = []
      transposeChunks xss = map head xss : transposeChunks (map tail xss)
      -- Recombine the 3 lines of each digit
      reassembleDigitLines :: [String] -> [String]
      reassembleDigitLines digitLines =
        [ concat [digitLines !! 0, digitLines !! 3, digitLines !! 6, digitLines !! 9],
          concat [digitLines !! 1, digitLines !! 4, digitLines !! 7, digitLines !! 10],
          concat [digitLines !! 2, digitLines !! 5, digitLines !! 8, digitLines !! 11]
        ]
      -- Recognize each digit in the line
      recognizedDigits = map (recognizeDigit . reassembleDigitLines) (transpose $ map (padToMultipleOfThree ' ') groupedChunks)
      padToMultipleOfThree :: Char -> [String] -> [String]
      padToMultipleOfThree padChar lines = map (\line -> line ++ replicate (3 - (length line `mod` 3)) padChar) lines
      transpose :: [[a]] -> [[a]]
      transpose [] = []
      transpose ([]:_) = []
      transpose xss = map head xss : transpose (map tail xss)
   in if not isValidLine
        then error "Invalid input size"
        else recognizedDigits

-- Main convert function
convert :: String -> String
convert input =
  let linesOfNumbers = lines input
   in intercalate "," (map convertLine (groupIntoFour linesOfNumbers))

groupIntoFour :: [String] -> [[String]]
groupIntoFour [] = []
groupIntoFour lines
  | length lines >= 4 = take 4 lines : groupIntoFour (drop 4 lines)
  | otherwise = take 4 lines : groupIntoFour (drop 4 lines)
