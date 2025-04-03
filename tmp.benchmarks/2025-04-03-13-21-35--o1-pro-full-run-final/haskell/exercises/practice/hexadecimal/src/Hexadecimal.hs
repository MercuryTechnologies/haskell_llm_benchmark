module Hexadecimal (hexToInt) where

import Data.Char (toLower, ord)

hexToInt :: String -> Int
hexToInt s =
  case mapM hexDigitToValue s of
    Nothing     -> 0
    Just digits -> foldl (\acc d -> acc * 16 + d) 0 digits

hexDigitToValue :: Char -> Maybe Int
hexDigitToValue c =
  let lower = toLower c
  in if lower >= '0' && lower <= '9'
     then Just (ord lower - ord '0')
     else if lower >= 'a' && lower <= 'f'
     then Just (10 + (ord lower - ord 'a'))
     else Nothing
