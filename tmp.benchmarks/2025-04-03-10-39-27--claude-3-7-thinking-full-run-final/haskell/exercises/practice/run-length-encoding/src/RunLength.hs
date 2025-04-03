module RunLength (decode, encode) where

import Data.Char (isDigit)
import Data.List (group)

decode :: String -> String
decode [] = []
decode s =
  case span isDigit s of
    ("", c:rest) -> c : decode rest  -- No count prefix, include character once
    (countStr, c:rest) -> replicate (read countStr) c ++ decode rest  -- Repeat character by count
    (_, []) -> []  -- End of string

encode :: String -> String
encode = concatMap encodeRun . group
  where
    encodeRun xs@(x:_)
      | length xs == 1 = [x]  -- Single character, no count needed
      | otherwise      = show (length xs) ++ [x]
    encodeRun [] = []  -- Empty case (shouldn't happen with group)
