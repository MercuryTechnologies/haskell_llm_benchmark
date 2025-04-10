module CryptoSquare (encode) where

import Data.Char (isAlpha, toLower)
import Data.List (transpose)

-- Normalize the input text
normalize :: String -> String
normalize = map toLower . filter isAlpha

-- Calculate the number of columns and rows
calcDimensions :: Int -> (Int, Int)
calcDimensions len = let c = ceiling $ sqrt $ fromIntegral len
                         r = (len + c - 1) `div` c
                     in (c, r)

-- Encode the normalized text
encode :: String -> String
encode xs = let normalized = normalize xs
                len = length normalized
                (c, r) = calcDimensions len
                rect = take (r * c) $ normalized ++ repeat ' '
                chunks = [take c $ drop (i*r) rect | i <- [0..c-1]]
                encoded = unwords $ map (take r) $ transpose chunks
            in encoded
