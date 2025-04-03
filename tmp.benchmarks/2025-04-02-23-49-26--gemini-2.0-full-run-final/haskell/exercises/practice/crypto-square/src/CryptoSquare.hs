module CryptoSquare (encode) where

import Data.Char (isLetter, toLower)
import Data.List (transpose)
import Data.Maybe (fromMaybe)

encode :: String -> String
encode xs =
  let normalized = normalize xs
      len = length normalized
      (r, c) = dimensions len
      padded = pad normalized (r * c)
      rectangle = chunksOf c padded
      encoded = concat $ transpose rectangle
      chunkedEncoded = chunksOf r encoded
      paddedChunkedEncoded = map (\chunk -> chunk ++ replicate (r - length chunk) ' ') chunkedEncoded
   in unwords paddedChunkedEncoded

normalize :: String -> String
normalize = map toLower . filter isLetter

dimensions :: Int -> (Int, Int)
dimensions len
  | len == 0 = (1, 1)
  | otherwise = head $ filter (\(r, c) -> r * c >= len && c >= r && c - r <= 1) $ [(r, c) | r <- [1 .. len], c <- [1 .. len]]

pad :: String -> Int -> String
pad str len = str ++ replicate (len - length str) ' '

chunksOf :: Int -> String -> [String]
chunksOf _ [] = []
chunksOf n xs = let (chunk, rest) = splitAt n xs in chunk : chunksOf n rest
