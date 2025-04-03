module CryptoSquare (encode) where

import Data.Char (isAlphaNum, toLower)
import Data.List (transpose)

-- Normalize the input by removing non-alphanumeric characters and converting to lowercase
normalize :: String -> String
normalize = map toLower . filter isAlphaNum

-- Calculate the number of columns and rows for the rectangle
calculateDimensions :: Int -> (Int, Int)
calculateDimensions len = head [(r, c) | c <- [1..], let r = (len + c - 1) `div` c, r <= c, r * c >= len]

-- Encode the message using the square code method
encode :: String -> String
encode xs = unwords . transpose . padRows c . chunk c $ normalized
  where
    normalized = normalize xs
    (r, c) = calculateDimensions (length normalized)

-- Break the string into chunks of the given size
chunk :: Int -> String -> [String]
chunk _ [] = []
chunk n s = take n s : chunk n (drop n s)

-- Pad the rows to ensure they form a perfect rectangle
padRows :: Int -> [String] -> [String]
padRows c rows = map (\row -> row ++ replicate (c - length row) ' ') rows
