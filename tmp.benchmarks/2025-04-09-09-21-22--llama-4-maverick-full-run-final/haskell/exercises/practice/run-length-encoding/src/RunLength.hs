module RunLength (decode, encode) where

import Data.Char (isDigit)

-- Encode a string using run-length encoding
encode :: String -> String
encode [] = []
encode (x:xs) = encodeHelper 1 x xs

encodeHelper :: Int -> Char -> String -> String
encodeHelper count char [] = if count == 1 then [char] else show count ++ [char]
encodeHelper count char (x:xs)
  | char == x = encodeHelper (count + 1) char xs
  | otherwise = (if count == 1 then [char] else show count ++ [char]) ++ encodeHelper 1 x xs

-- Decode a run-length encoded string
decode :: String -> String
decode [] = []
decode xs = decodeHelper xs

decodeHelper :: String -> String
decodeHelper [] = []
decodeHelper xs = let (count, rest) = span isDigit xs
                  in replicate (if null count then 1 else read count) (head rest) ++ decodeHelper (drop 1 rest)

-- Handle the case where the input string starts with a non-digit character
decodeHelper (x:xs) | not (isDigit x) = x : decodeHelper xs
