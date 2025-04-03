module Affine (decode, encode) where

import Data.Char (isAlphaNum, isLower, ord, toLower)

-- Function to calculate the modular multiplicative inverse
modInverse :: Int -> Int -> Maybe Int
modInverse a m =
  case filter (\x -> (a * x) `mod` m == 1) [0 .. m - 1] of
    [x] -> Just x
    _   -> Nothing

-- Function to check if two numbers are coprime
coprime :: Int -> Int -> Bool
coprime a m = gcd a m == 1

-- Encryption function
encode :: (Int, Int) -> String -> Maybe String
encode (a, b) plainText
  | not (coprime a 26) = Nothing
  | otherwise =
    Just $ unwords $ chunksOf 5 $ map encryptChar $ filter isAlphaNumLower $ map toLower plainText
  where
    encryptChar c
      | isLower c = toEncryptedChar c
      | otherwise = c
    toEncryptedChar c = toChar $ (a * (fromChar c) + b) `mod` 26
    fromChar c = ord c - ord 'a'
    toChar i = toLower $ toEnum (ord 'a' + i) :: Char
    isAlphaNumLower c = isAlphaNum c && isLower c

-- Decryption function
decode :: (Int, Int) -> String -> Maybe String
decode (a, b) cipherText
  | not (coprime a 26) = Nothing
  | otherwise =
    case modInverse a 26 of
      Just aInverse ->
        Just $ map decryptChar $ filter isAlphaNumLower $ map toLower cipherText
      Nothing -> Nothing
  where
    aInverse = case modInverse a 26 of
                 Just inv -> inv
                 Nothing  -> 0  -- Dummy value, the 'Nothing' case is already handled above
    decryptChar c
      | isLower c = toDecryptedChar c
      | otherwise = c
    toDecryptedChar c = toChar $ (aInverse * (fromChar c - b)) `mod` 26
    fromChar c = ord c - ord 'a'
    toChar i = toLower $ toEnum (ord 'a' + i) :: Char
    isAlphaNumLower c = isAlphaNum c && isLower c

-- Helper function to split a list into chunks of a given size
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (chunk, rest) = splitAt n xs in chunk : chunksOf n rest
