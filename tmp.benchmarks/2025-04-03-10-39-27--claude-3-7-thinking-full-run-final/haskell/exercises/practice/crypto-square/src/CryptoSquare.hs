module CryptoSquare (encode) where

import Data.Char (isAlphaNum, toLower)
import Data.List (transpose)

-- Normalize the input text
normalize :: String -> String
normalize = map toLower . filter isAlphaNum

-- Calculate the dimensions of the rectangle (r rows, c columns)
dimensions :: Int -> (Int, Int)
dimensions len = head [(r, c) | c <- [1..], 
                                let r = ceiling $ fromIntegral len / fromIntegral c, 
                                r * c >= len, 
                                c >= r, 
                                c - r <= 1]

-- Encode the input text
encode :: String -> String
encode "" = ""
encode text = 
  let normalized = normalize text
      len = length normalized
      (r, c) = dimensions len
      paddedText = normalized ++ replicate (r * c - len) ' '
      rows = chunksOf c paddedText
      columns = transpose rows
  in unwords columns
  where
    chunksOf _ [] = []
    chunksOf n xs = take n xs : chunksOf n (drop n xs)
