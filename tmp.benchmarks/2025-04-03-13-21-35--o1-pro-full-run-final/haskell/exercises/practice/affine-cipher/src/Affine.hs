module Affine (decode, encode) where

import Data.Char (isAlpha, isDigit, toLower, ord, chr)
import Data.List (intercalate)

-- | Check if two integers are coprime:
isCoprime :: Int -> Int -> Bool
isCoprime a m = gcd a m == 1

-- | Try computing the modular multiplicative inverse of 'a' modulo 'm'.
--   This function returns Nothing if 'a' and 'm' are not coprime.
modInverse :: Int -> Int -> Maybe Int
modInverse a m
  | not (isCoprime a m) = Nothing
  | otherwise           =
      let possible = [ x | x <- [1..m-1], (a*x) `mod` m == 1 ]
      in if null possible then Nothing else Just (head possible)

-- | Convert a lower/upper-case letter to its [0..25] index.
charToIndex :: Char -> Int
charToIndex c = (ord (toLower c) - ord 'a') `mod` 26

-- | Convert an index in [0..25] to a lower-case letter.
indexToChar :: Int -> Char
indexToChar i = chr (ord 'a' + (i `mod` 26))

-- | Remove spaces/punctuation by keeping only letters and digits
filterValid :: String -> String
filterValid = filter (\c -> isAlpha c || isDigit c)

-- | Chunk a string into groups of size n, space-separated.
chunkify :: Int -> String -> String
chunkify _ [] = []
chunkify n s =
  let (group, rest) = splitAt n s
  in if null rest
     then group
     else group ++ " " ++ chunkify n rest

-- | Encode a string with the given (a, b) key using the Affine cipher.
--   Return Nothing if a and 26 are not coprime.
encode :: (Int, Int) -> String -> Maybe String
encode (a, b) plainText
  | not (isCoprime a 26) = Nothing
  | otherwise = Just $ chunkify 5 (map transform (filterValid plainText))
  where
    transform c
      | isAlpha c = indexToChar ((a * charToIndex c + b) `mod` 26)
      | otherwise = c  -- digits pass through unchanged

-- | Decode a string with the given (a, b) key using the Affine cipher.
--   Return Nothing if a and 26 are not coprime.
decode :: (Int, Int) -> String -> Maybe String
decode (a, b) cipherText = do
  inv <- modInverse a 26        -- fail (Nothing) if not coprime
  let noSpaces = filterValid cipherText
  let result   = map (transform inv) noSpaces
  return result
  where
    transform inv c
      | isAlpha c =
          let y = charToIndex c
              x = (inv * (y - b)) `mod` 26
          in indexToChar x
      | otherwise = c  -- digits remain unchanged
