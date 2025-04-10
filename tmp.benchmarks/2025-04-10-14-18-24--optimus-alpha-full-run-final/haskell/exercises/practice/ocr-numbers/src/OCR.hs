module OCR (convert) where

import Data.List (transpose, intercalate)
import Data.Maybe (fromMaybe)

-- Map of digit patterns to their string representation
digitMap :: [(String, Char)]
digitMap =
    [ (" _ | ||_|   ", '0')
    , ("     |  |   ", '1')
    , (" _  _||_    ", '2')
    , (" _  _| _|   ", '3')
    , ("   |_|  |   ", '4')
    , (" _ |_  _|   ", '5')
    , (" _ |_ |_|   ", '6')
    , (" _   |  |   ", '7')
    , (" _ |_||_|   ", '8')
    , (" _ |_| _|   ", '9')
    ]

-- Given a 3x4 block, return the corresponding digit or '?'
parseDigit :: [String] -> Char
parseDigit block
    | length block /= 4 || any ((/= 3) . length) block = '?'
    | otherwise = fromMaybe '?' $ lookup (concat block) digitMap

-- Split a string into chunks of n
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- Parse a single line of OCR (4 rows of text)
parseLine :: [String] -> String
parseLine rows
    | length rows /= 4 = error "Invalid number of rows"
    | any (\r -> length r `mod` 3 /= 0) rows = error "Invalid row width"
    | otherwise =
        let numDigits = length (head rows) `div` 3
            digitBlocks = [ [ take 3 (drop (i*3) row) | row <- rows ] | i <- [0..numDigits-1] ]
        in map parseDigit digitBlocks

-- Main convert function
convert :: String -> String
convert xs =
    let allRows = lines xs
        -- Each number is 4 rows high
        rowGroups = chunksOf 4 allRows
    in
        if any (\g -> length g /= 4) rowGroups
            then error "Invalid input: not a multiple of 4 rows"
            else
                let results = map parseLine rowGroups
                in intercalate "," results
