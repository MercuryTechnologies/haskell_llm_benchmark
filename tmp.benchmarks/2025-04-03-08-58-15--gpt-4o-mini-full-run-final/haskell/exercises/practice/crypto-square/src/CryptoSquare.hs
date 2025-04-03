module CryptoSquare (encode) where

import Data.Char (isAlpha, toLower)
import Data.List (transpose)

encode :: String -> String
encode xs = unwords $ chunks encodedMessage r
  where
    normalized = filter isAlpha (map toLower xs)
    len = length normalized
    r = floor (sqrt (fromIntegral len))
    c = ceiling (fromIntegral len / fromIntegral r)
    paddedLength = r * c
    paddedMessage = normalized ++ replicate (paddedLength - len) ' '
    rows = chunks paddedMessage c
    encodedMessage = concatMap (map snd) $ transpose $ zip [0..] rows

chunks :: [a] -> Int -> [[a]]
chunks [] _ = []
chunks xs n = take n xs : chunks (drop n xs) n
