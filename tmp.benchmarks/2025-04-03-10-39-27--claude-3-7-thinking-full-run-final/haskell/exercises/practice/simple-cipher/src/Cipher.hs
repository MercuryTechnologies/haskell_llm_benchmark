module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where

import Data.Char (ord, chr, isLower)
import Data.List (iterate')
import Data.Time.Clock.POSIX (getPOSIXTime)

-- Convert a character to its 0-25 index
charToIndex :: Char -> Int
charToIndex c = ord c - ord 'a'

-- Convert a 0-25 index back to a character
indexToChar :: Int -> Char
indexToChar i = chr (i + ord 'a')

-- Apply the Caesar shift to a character with a given key character
shiftChar :: Char -> Char -> Char
shiftChar keyChar plainChar
    | isLower plainChar = indexToChar ((charToIndex plainChar + charToIndex keyChar) `mod` 26)
    | otherwise = plainChar

-- Apply the reverse Caesar shift to a character with a given key character
unshiftChar :: Char -> Char -> Char
unshiftChar keyChar cipherChar
    | isLower cipherChar = indexToChar ((charToIndex cipherChar - charToIndex keyChar + 26) `mod` 26)
    | otherwise = cipherChar

-- Encode a message using a key
caesarEncode :: String -> String -> String
caesarEncode key text 
    | null key = error "Key cannot be empty for encoding. Use caesarEncodeRandom for a random key."
    | all isLower key = zipWith shiftChar (cycle key) text
    | otherwise = error "Key must contain only lowercase letters"

-- Decode a message using a key
caesarDecode :: String -> String -> String
caesarDecode key encodedText 
    | null key = error "Key cannot be empty for decoding"
    | all isLower key = zipWith unshiftChar (cycle key) encodedText
    | otherwise = error "Key must contain only lowercase letters"

-- Generate a pseudorandom key of given length
generateKey :: Int -> IO String
generateKey len = do
    -- Use current time to seed a simple pseudorandom generator
    timestamp <- round . (* 1000000) <$> getPOSIXTime
    let 
        -- Simple linear congruential generator
        lcg seed = (1103515245 * seed + 12345) `mod` 2147483648
        -- Generate a sequence of pseudorandom numbers
        values = take len $ map (`mod` 26) $ tail $ iterate lcg timestamp
        -- Convert to lowercase letters
        key = map indexToChar values
    return key

-- Generate a key and encode a message
caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = do
    key <- generateKey 100
    return (key, caesarEncode key text)
