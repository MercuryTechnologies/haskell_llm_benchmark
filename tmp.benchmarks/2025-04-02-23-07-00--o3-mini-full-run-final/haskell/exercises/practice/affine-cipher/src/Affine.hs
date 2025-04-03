module Affine (decode, encode) where

import Data.Char (isAlpha, isDigit, isLetter, toLower, ord, chr)

-- Calculate greatest common divisor
gcd' :: Int -> Int -> Int
gcd' a 0 = abs a
gcd' a b = gcd' b (a `mod` b)

-- Extended Euclidean algorithm to compute the modular inverse
extendedGCD :: Int -> Int -> (Int, Int, Int)
extendedGCD 0 b = (b, 0, 1)
extendedGCD a b =
    let (g, x1, y1) = extendedGCD (b `mod` a) a
        x = y1 - (b `div` a) * x1
        y = x1
    in (g, x, y)

-- Compute the modular multiplicative inverse of a modulo m.
modInv :: Int -> Int -> Maybe Int
modInv a m =
    let (g, x, _) = extendedGCD a m
    in if g == 1 then Just (x `mod` m) else Nothing

-- Group a string into chunks of 5 characters separated by spaces.
groupFive :: String -> String
groupFive = unwords . chunk 5
  where
    chunk _ [] = []
    chunk n xs =
      let (first, rest) = splitAt n xs
      in first : chunk n rest

-- | Encrypt the plain text using the affine cipher.
-- Digits are not encrypted, letters are converted to lowercase and encrypted,
-- and any other characters (such as punctuation and spaces) are omitted.
-- The result is grouped in chunks of 5 characters.
encode :: (Int, Int) -> String -> Maybe String
encode (a, b) plainText
  | gcd' a 26 /= 1 = Nothing
  | otherwise = Just $ groupFive $ map encryptChar filtered
  where
    -- Filter the plain text: convert letters to lowercase, keep digits.
    filtered = [ if isAlpha c then toLower c else c | c <- plainText, isAlpha c || isDigit c ]
    encryptChar c
      | isLetter c =
          let i = ord c - ord 'a'
              newIndex = (a * i + b) `mod` 26
          in chr (newIndex + ord 'a')
      | otherwise = c

-- | Decrypt the cipher text using the affine cipher.
-- Spaces are removed from the cipher text. Letters are decrypted,
-- digits remain unchanged.
decode :: (Int, Int) -> String -> Maybe String
decode (a, b) cipherText
  | gcd' a 26 /= 1 = Nothing
  | otherwise = case modInv a 26 of
      Nothing -> Nothing
      Just aInv -> Just $ map decryptChar filtered
        where
          -- Remove spaces from the cipher text.
          filtered = [ c | c <- cipherText, c /= ' ' ]
          decryptChar c
            | isLetter c =
                let i = ord c - ord 'a'
                    newIndex = (aInv * (i - b)) `mod` 26
                    adjIndex = if newIndex < 0 then newIndex + 26 else newIndex
                in chr (adjIndex + ord 'a')
            | otherwise = c
