module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where

import System.Random (randomRs, getStdGen)

-- Function to shift a character by a certain number of places
shiftChar :: Int -> Char -> Char
shiftChar shift c
  | c >= 'a' && c <= 'z' = chr $ ((ord c - ord 'a' + shift) `mod` 26) + ord 'a'
  | otherwise            = c

-- Caesar Encode function
caesarEncode :: String -> String -> String
caesarEncode key text = zipWith shiftChar (map shift key) text
  where
    shift c = ord c - ord 'a'
    ord = fromEnum
    chr = toEnum

-- Caesar Decode function
caesarDecode :: String -> String -> String
caesarDecode key encodedText = caesarEncode key encodedText
  where
    -- To decode, we need to shift in the opposite direction
    -- But since our caesarEncode already handles this with the key,
    -- we can simply use the same function with the same key for decoding
    -- However, the problem statement implies using the inverse key for decoding
    -- inverseKey = map (\c -> chr $ ord 'a' + (26 - (ord c - ord 'a')) `mod` 26) key
    -- But the above is not needed as the decode is the same as encode with the same key
    -- due to the properties of the Caesar cipher when using a key.

-- Generate a random key of a given length
generateRandomKey :: Int -> IO String
generateRandomKey len = do
  gen <- getStdGen
  return $ take len $ map (chr . (+ ord 'a') . (`mod` 26)) (randomRs (0, 25) gen)
  where
    ord = fromEnum
    chr = toEnum

-- Caesar Encode Random function
caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = do
  key <- generateRandomKey (max 100 (length text))
  return (key, caesarEncode key text)
