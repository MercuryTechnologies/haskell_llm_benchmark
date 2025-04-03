module OCR (convert) where

import Data.List (intercalate, transpose)

convert :: String -> String
convert input
  | not (validSize input) = error "Input is the incorrect size."
  | otherwise = intercalate "," $ map (map recognizeDigit . splitDigits) (splitLines input)

validSize :: String -> Bool
validSize input = all (\line -> length line `mod` 3 == 0) (lines input) && length (lines input) `mod` 4 == 0

splitLines :: String -> [String]
splitLines [] = []
splitLines input = let (chunk, rest) = splitAt 4 (lines input)
                   in unlines chunk : splitLines (unlines rest)

splitDigits :: String -> [String]
splitDigits input = let lns = lines input
                        chunks = map (chunksOf 3) lns
                    in map unlines $ transpose chunks

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (chunk, rest) = splitAt n xs
                in chunk : chunksOf n rest

recognizeDigit :: String -> Char
recognizeDigit " _ \n| |\n|_|\n   \n" = '0'
recognizeDigit "   \n  |\n  |\n   \n" = '1'
recognizeDigit " _ \n _|\n|_ \n   \n" = '2'
recognizeDigit " _ \n _|\n _|\n   \n" = '3'
recognizeDigit "   \n|_|\n  |\n   \n" = '4'
recognizeDigit " _ \n|_ \n _|\n   \n" = '5'
recognizeDigit " _ \n|_ \n|_|\n   \n" = '6'
recognizeDigit " _ \n  |\n  |\n   \n" = '7'
recognizeDigit " _ \n|_|\n|_|\n   \n" = '8'
recognizeDigit " _ \n|_|\n _|\n   \n" = '9'
recognizeDigit _ = '?'
