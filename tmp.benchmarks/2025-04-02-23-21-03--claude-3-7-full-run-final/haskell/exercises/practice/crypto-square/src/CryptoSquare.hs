module CryptoSquare (encode) where

import Data.Char (isAlphaNum, toLower)
import Data.List (transpose)

encode :: String -> String
encode "" = ""
encode xs = unwords chunks
  where
    normalized = map toLower $ filter isAlphaNum xs
    len = length normalized
    c = ceiling $ sqrt $ fromIntegral len  -- number of columns
    r = ceiling $ fromIntegral len / fromIntegral c  -- number of rows
    
    -- Create rows by splitting the normalized text
    rows = chunksOf c $ normalized ++ replicate (r * c - len) ' '
    
    -- Read down the columns (transpose the rectangle)
    columns = transpose rows
    
    -- Group into chunks of length r
    chunks = chunksOf r $ concat columns

-- Helper function to split a list into chunks of specified size
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)
