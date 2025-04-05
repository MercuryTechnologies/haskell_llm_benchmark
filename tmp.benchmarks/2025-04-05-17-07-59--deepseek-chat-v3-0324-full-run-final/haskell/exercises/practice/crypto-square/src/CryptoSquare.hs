module CryptoSquare (encode) where

import Data.Char (isAlphaNum, toLower)
import Data.List (transpose)

encode :: String -> String
encode = unwords . transpose . splitIntoRows . normalize

normalize :: String -> String
normalize = map toLower . filter isAlphaNum

calculateDimensions :: Int -> (Int, Int)
calculateDimensions len
    | len == 0 = (0, 0)
    | otherwise = 
        let r = floor (sqrt (fromIntegral len))
            c = if r * r >= len then r else r + 1
            (r', c') = if c * r >= len && c - r <= 1 
                       then (r, c) 
                       else (r + 1, c)
        in (r', c')

splitIntoRows :: String -> [String]
splitIntoRows [] = []
splitIntoRows str = 
    let len = length str
        (r, c) = calculateDimensions len
        padded = str ++ replicate (r * c - len) ' '
    in [ take c (drop (i * c) padded) | i <- [0..r-1] ]
