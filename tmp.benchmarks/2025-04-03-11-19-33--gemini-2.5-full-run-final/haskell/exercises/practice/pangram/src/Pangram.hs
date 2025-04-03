module Pangram (isPangram) where

import Data.Char (toLower)

isPangram :: String -> Bool
isPangram text = all (\c -> c `elem` lowerText) alphabet
  where
    lowerText = map toLower text
    alphabet = ['a'..'z']
