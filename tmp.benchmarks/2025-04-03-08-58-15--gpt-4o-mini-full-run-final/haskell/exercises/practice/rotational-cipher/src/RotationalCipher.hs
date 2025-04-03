module RotationalCipher (rotate) where

import qualified Data.Text as T
import           Data.Text (Text)

rotate :: Int -> Text -> Text
rotate key text = T.pack $ map (rotateChar key) (T.unpack text)

rotateChar :: Int -> Char -> Char
rotateChar key char
  | 'a' <= char && char <= 'z' = shift char 'a'
  | 'A' <= char && char <= 'Z' = shift char 'A'
  | otherwise                   = char
  where
    shift c base = toEnum $ (fromEnum c - fromEnum base + key) `mod` 26 + fromEnum base
