module Affine (decode, encode) where

import Data.Char ( isAlpha, isDigit, toLower, ord, chr )
import Data.List ( unfoldr )

alphabetSize :: Int
alphabetSize = 26

isCoprime :: Int -> Int -> Bool
isCoprime x y = gcd x y == 1

modInverse :: Int -> Int -> Int
modInverse a m = head [ x | x <- [1..m-1], (a * x) `mod` m == 1 ]

clean :: Char -> Maybe Char
clean c
  | isAlpha c = Just (toLower c)
  | isDigit c = Just c
  | otherwise = Nothing

encode :: (Int, Int) -> String -> Maybe String
encode (a, b) plainText
  | not (isCoprime a alphabetSize) = Nothing
  | otherwise =
      let filtered    = [ c' | c <- plainText, Just c' <- [clean c] ]
          transformed = map (encodeChar a b) filtered
          chunked     = chunksOf 5 transformed
      in Just (unwords chunked)

encodeChar :: Int -> Int -> Char -> Char
encodeChar _ _ c
  | isDigit c = c
encodeChar a b c =
  let i = ord c - ord 'a'
      e = (a * i + b) `mod` alphabetSize
  in chr (e + ord 'a')

chunksOf :: Int -> String -> [String]
chunksOf n = unfoldr (\xs -> if null xs then Nothing else Just (splitAt n xs))

decode :: (Int, Int) -> String -> Maybe String
decode (a, b) cipherText
  | not (isCoprime a alphabetSize) = Nothing
  | otherwise =
      let invA     = modInverse a alphabetSize
          filtered = [ c' | c <- filter (/= ' ') cipherText, Just c' <- [clean c] ]
      in Just (map (decodeChar invA b) filtered)

decodeChar :: Int -> Int -> Char -> Char
decodeChar _ _ c
  | isDigit c = c
decodeChar invA b c =
  let i = ord c - ord 'a'
      d = (invA * (i - b)) `mod` alphabetSize
  in chr (d + ord 'a')
