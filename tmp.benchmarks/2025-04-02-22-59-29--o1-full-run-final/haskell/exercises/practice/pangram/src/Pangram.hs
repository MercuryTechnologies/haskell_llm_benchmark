module Pangram (isPangram) where

import Data.Char (isAlpha, toLower)

isPangram :: String -> Bool
isPangram text =
    let letters = [toLower c | c <- text, isAlpha c]
    in all (`elem` letters) ['a'..'z']
