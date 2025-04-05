module CryptoSquare (encode) where

import Data.Char (isAlphaNum, toLower)
import Data.List (transpose)

encode :: String -> String
encode xs = unwords paddedColumns
  where
    normalized = map toLower $ filter isAlphaNum xs
    len = length normalized
    (r, c) = rectangleSize len
    rows = chunksOf c normalized
    -- pad rows to length c
    paddedRows = map (\row -> row ++ replicate (c - length row) ' ') rows
    columns = transpose paddedRows
    paddedColumns = map padToR columns

    padToR col = col ++ replicate (r - length col) ' '

rectangleSize :: Int -> (Int, Int)
rectangleSize len = head [(r, c) | c <- [1..], let r = ceiling (fromIntegral len / fromIntegral c), c >= r, c - r <= 1, r * c >= len]

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (first, rest) = splitAt n xs in first : chunksOf n rest
