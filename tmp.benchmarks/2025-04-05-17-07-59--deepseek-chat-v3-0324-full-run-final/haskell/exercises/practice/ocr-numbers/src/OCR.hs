module OCR (convert) where

import Data.List (intercalate, transpose)

-- Define the patterns for each digit
digitPatterns :: [(String, Char)]
digitPatterns =
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

-- Custom chunksOf implementation
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- Convert a single 3x4 digit to character
convertDigit :: String -> Char
convertDigit digit
    | length digit /= 12 = '?'
    | otherwise = case lookup digit digitPatterns of
        Just c  -> c
        Nothing -> '?'

-- Convert a line of digits to a string
convertLine :: String -> String
convertLine input
    | null input = ""
    | length (head lines) /= 3 = '?' : convertLine (drop 3 input)
    | otherwise = map convertDigit (map concat (transpose (map (chunksOf 3) lines)))
    where
        lines = take 4 (input : rest)
        rest = tail (splitIntoLines input)
        splitIntoLines = chunksOf (length input `div` 4)

-- Main conversion function
convert :: String -> String
convert input
    | null input = ""
    | length input `mod` 4 /= 0 = "?"  -- Invalid height
    | otherwise = intercalate "," (map convertLine (filter (not . null) (splitIntoLines input)))
    where
        splitIntoLines = chunksOf (length input `div` (length (lines input) `div` 4))
