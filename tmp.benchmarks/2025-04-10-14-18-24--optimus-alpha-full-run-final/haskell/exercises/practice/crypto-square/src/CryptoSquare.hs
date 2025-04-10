module CryptoSquare (encode) where

import Data.Char (isAlphaNum, toLower)
import Data.List (transpose)

-- Normalize the input: remove non-alphanum and downcase
normalize :: String -> String
normalize = map toLower . filter isAlphaNum

-- Compute (rows, cols) for the rectangle
rectangleSize :: Int -> (Int, Int)
rectangleSize len = head [(r, c) | c <- [1..], let r = ceiling (fromIntegral len / fromIntegral c), c >= r, c - r <= 1, r * c >= len]

-- Pad a string to a given length with spaces
padTo :: Int -> String -> String
padTo n s = s ++ replicate (n - length s) ' '

encode :: String -> String
encode xs =
    let norm = normalize xs
        len = length norm
        (r, c) = rectangleSize len
        rows = [take c (drop (i * c) norm) | i <- [0..r-1]]
        paddedRows = map (padTo c) rows
        columns = transpose paddedRows
        -- Each column is a chunk, pad to r if needed
        chunks = map (padTo r) (map (take r) columns)
    in unwords chunks
