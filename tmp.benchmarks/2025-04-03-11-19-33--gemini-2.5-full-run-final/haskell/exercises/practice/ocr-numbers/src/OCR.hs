module OCR (convert) where

import Data.List (transpose, intercalate)
import Data.Map (Map, fromList, findWithDefault)
-- No Data.List.Split needed as chunksOf is implemented below.

-- Helper function: chunksOf
-- Splits a list into chunks of a given size.
-- Similar to the function from Data.List.Split.
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs
  -- We expect positive chunk sizes (3 or 4 in this module).
  | n <= 0    = error "OCR.chunksOf: chunk size must be positive"
  | otherwise = let (chunk, rest) = splitAt n xs
                in chunk : chunksOf n rest

-- Main conversion function called by tests or users.
-- Takes the raw multiline string input and returns the comma-separated OCR result.
convert :: String -> String
convert input = recognizeLines $ parseLines input

-- Parses the input string into blocks of 4 lines each.
-- Each block corresponds to one line of recognizable numbers.
parseLines :: String -> [[String]]
parseLines = chunksOf 4 . lines

-- Recognizes numbers line by line (each line being a 4-row block).
-- Joins the results for each line with commas.
recognizeLines :: [[String]] -> String
recognizeLines = intercalate "," . map recognizeLine

-- Recognizes the sequence of digits represented in a single 4-line block.
recognizeLine :: [String] -> String
recognizeLine block
    -- A valid block must have exactly 4 lines for consistent processing.
    | length block /= 4 = "?" -- Represent invalid block structure as unrecognizable.
    | otherwise =
        let patternLines = take 3 block -- Digits are defined by the first 3 lines.
            -- Get the lengths of the relevant lines.
            lineLengths = map length patternLines
        in case lineLengths of
             -- Check structure: Must have 3 lines, all non-empty, all same length, length multiple of 3.
             (l1:l2:l3:[]) | l1 == l2 && l2 == l3 && l1 > 0 && l1 `mod` 3 == 0 ->
                 -- If structure is valid, proceed to extract patterns and recognize digits.
                 recognizeDigits $ extractDigitPatterns patternLines
             -- Otherwise, the line structure is malformed (e.g., wrong line count,
             -- inconsistent lengths, zero length, or length not multiple of 3).
             _ -> "?" -- Represent malformed line structure as unrecognizable for the whole line.

-- Extracts individual digit patterns from the 3 lines defining them.
-- Assumes input `patternLines` contains 3 strings of the same, non-zero length,
-- which is a multiple of 3, based on checks in `recognizeLine`.
extractDigitPatterns :: [String] -> [String]
extractDigitPatterns patternLines =
    -- Split each of the 3 pattern lines into 3-character wide segments.
    let segmentsPerLine = map (chunksOf 3) patternLines
    -- Transpose the list of lists of segments. Now each inner list contains the
    -- three vertical segments corresponding to one digit position.
    -- e.g., [[" _ "," _ "],["| |","| |"],["|_|","|_|"]] -> [[" _ ","| |","|_|"], [" _ ","| |","|_|"]]
        segmentsPerDigit = transpose segmentsPerLine
    -- Concatenate the segments (lines) for each digit to form the final pattern string.
    -- e.g., [" _ ","| |","|_|"] becomes " _ | ||_|"
    in map concat segmentsPerDigit

-- Recognizes a list of digit pattern strings, returning a string composed of digits ('0'-'9') or '?'.
recognizeDigits :: [String] -> String
recognizeDigits = map recognizeDigit

-- Recognizes a single digit pattern string using the predefined map.
-- Returns the corresponding digit character or '?' if the pattern is unknown.
recognizeDigit :: String -> Char
recognizeDigit pattern = findWithDefault '?' pattern digitMap

-- The map defining the visual pattern string for each digit character ('0' through '9').
-- The key is the pattern string formed by concatenating the 3 relevant lines for the digit.
-- The value is the character representation of the digit.
digitMap :: Map String Char
digitMap = fromList
    [ (" _ | ||_|", '0') -- Zero
    , ("     |  |", '1') -- One
    , (" _  _||_ ", '2') -- Two
    , (" _  _| _|", '3') -- Three
    , ("   |_|  |", '4') -- Four
    , (" _ |_  _|", '5') -- Five
    , (" _ |_ |_|", '6') -- Six
    , (" _   |  |", '7') -- Seven
    , (" _ |_||_|", '8') -- Eight
    , (" _ |_| _|", '9') -- Nine
    ]
