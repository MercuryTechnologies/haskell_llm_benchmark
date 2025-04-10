module OCR (convert) where

import Data.List (transpose, intercalate)

digitPatterns :: [(String, Char)]
digitPatterns =
  [ (unlines [" _ ", "| |", "|_|", "   "], '0')
  , (unlines ["   ", "  |", "  |", "   "], '1')
  , (unlines [" _ ", " _|", "|_ ", "   "], '2')
  , (unlines [" _ ", " _|", " _|", "   "], '3')
  , (unlines ["   ", "|_|", "  |", "   "], '4')
  , (unlines [" _ ", "|_ ", " _|", "   "], '5')
  , (unlines [" _ ", "|_ ", "|_|", "   "], '6')
  , (unlines [" _ ", "  |", "  |", "   "], '7')
  , (unlines [" _ ", "|_|", "|_|", "   "], '8')
  , (unlines [" _ ", "|_|", " _|", "   "], '9')
  ]

gridToChar :: String -> Char
gridToChar grid = case lookup grid digitPatterns of
  Just c -> c
  Nothing -> '?'

parseGrid :: [String] -> [Char]
parseGrid rows = map gridToChar $ parseGrids rows

parseGrids :: [String] -> [String]
parseGrids rows = map unlines $ transpose $ map (map unwords) $ parseRowGroups $ map words rows

parseRowGroups :: [[String]] -> [[[String]]]
parseRowGroups rows = map (chunksOf 3) $ transpose $ chunksOf (length (head rows)) rows

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

convert :: String -> String
convert input
  | null input = ""
  | length lines' `mod` 4 /= 0 = error "Invalid input size"
  | any ((/= length (head lines')) . length) lines' = error "Invalid input size"
  | otherwise = intercalate "," $ map parseGrid $ groupGrids lines'
  where
    lines' = lines input
    groupGrids :: [String] -> [[String]]
    groupGrids [] = []
    groupGrids xs = take 4 xs : groupGrids (drop 4 xs)
