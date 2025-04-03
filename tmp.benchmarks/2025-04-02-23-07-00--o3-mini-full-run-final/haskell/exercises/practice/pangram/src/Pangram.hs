module Pangram (isPangram) where

import Data.Char (toLower)

isPangram :: String -> Bool
isPangram text =
  let letters = map toLower text
      filtered = filter (\c -> c >= 'a' && c <= 'z') letters
  in all (`elem` filtered) ['a'..'z']
