module OCR (convert) where

import Data.List (intercalate)

convert :: String -> String
convert input
    | length linesOfInput /= 4 = error "Input must have 4 rows."
    | otherwise = intercalate "," (map decodeNumber (splitIntoDigits linesOfInput))
  where
    linesOfInput = lines input

    splitIntoDigits :: [String] -> [[String]]
    splitIntoDigits rows = 
        let numCols = (length (head rows) + 1) `div` 3
        in [map (take 3) (map (drop (i * 3)) rows) | i <- [0..numCols-1]]

    decodeNumber :: [String] -> String
    decodeNumber digit
        | digit == ["   ", "  |", "  |", "   "] = "1"
        | digit == [" _ ", " _|", "|_ ", "   "] = "2"
        | digit == [" _ ", " _|", " _|", "   "] = "3"
        | digit == ["   ", "|_|", "  |", "   "] = "4"
        | digit == [" _ ", "|_ ", " _|", "   "] = "5"
        | digit == [" _ ", "|_ ", "|_|", "   "] = "6"
        | digit == [" _ ", "  |", "  |", "   "] = "7"
        | digit == [" _ ", "|_|", "|_|", "   "] = "8"
        | digit == [" _ ", "|_|", " _|", "   "] = "9"
        | digit == [" _ ", "| |", "|_|", "   "] = "0"
        | otherwise = "?"

    decodeNumbers :: [[String]] -> String
    decodeNumbers digits = concatMap decodeNumber digits
