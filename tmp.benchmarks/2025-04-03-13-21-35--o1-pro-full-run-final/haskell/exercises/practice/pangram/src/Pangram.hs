module Pangram (isPangram) where

import Data.Char (toLower, isAlpha)
import Data.List (nub)

isPangram :: String -> Bool
isPangram text =
    let letters = map toLower (filter isAlpha text)
    in all (`elem` letters) ['a'..'z']
