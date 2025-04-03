module OCR (convert) where

import Data.List (intercalate, transpose)

convert :: String -> String
convert input
  | null input = ""
  | any ((/= 4) . length) rowGroups = error "Invalid input: each number must be 4 rows high"
  | any ((/= 0) . mod (length . head) 3) rowGroups = error "Invalid input: each number must be 3 columns wide"
  | otherwise = intercalate "," $ map processRowGroup rowGroups
  where
    rows = lines input
    rowGroups = chunksOf 4 $ if last rows == "" then rows else rows ++ [""]

-- Process a group of 4 rows representing one or more digits
processRowGroup :: [String] -> String
processRowGroup rows
  | length rows /= 4 = error "Invalid input: each number must be 4 rows high"
  | any null rows = error "Invalid input: empty row detected"
  | otherwise = map digitToChar digitPatterns
  where
    -- Ensure all rows have the same length by padding with spaces
    maxLen = maximum $ map length rows
    paddedRows = map (\r -> r ++ replicate (maxLen - length r) ' ') rows
    -- Group the rows into 3-column chunks
    digitPatterns = transpose $ map (chunksOf 3) paddedRows

-- Convert a 4x3 pattern to its corresponding digit character
digitToChar :: [String] -> Char
digitToChar pattern
  | pattern == [" _ ", "| |", "|_|", "   "] = '0'
  | pattern == ["   ", "  |", "  |", "   "] = '1'
  | pattern == [" _ ", " _|", "|_ ", "   "] = '2'
  | pattern == [" _ ", " _|", " _|", "   "] = '3'
  | pattern == ["   ", "|_|", "  |", "   "] = '4'
  | pattern == [" _ ", "|_ ", " _|", "   "] = '5'
  | pattern == [" _ ", "|_ ", "|_|", "   "] = '6'
  | pattern == [" _ ", "  |", "  |", "   "] = '7'
  | pattern == [" _ ", "|_|", "|_|", "   "] = '8'
  | pattern == [" _ ", "|_|", " _|", "   "] = '9'
  | otherwise = '?'

-- Split a list into chunks of specified size
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)
