module Pangram (isPangram) where

import Data.Char (toLower)
import Data.List (nub)

isPangram :: String -> Bool
isPangram text = length letters == 26
  where
    letters = nub $ filter (`elem` ['a'..'z']) $ map toLower text
