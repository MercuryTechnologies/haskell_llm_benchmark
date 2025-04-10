module Proverb(recite) where

import Data.List (intercalate)

recite :: [String] -> String
recite [] = ""
recite [x] = "And all for the want of a " ++ x ++ "."
recite xs = 
    let pairs = zip xs (tail xs)
        lines = [ "For want of a " ++ a ++ " the " ++ b ++ " was lost." | (a, b) <- pairs ]
        final = "And all for the want of a " ++ head xs ++ "."
    in intercalate "\n" (lines ++ [final])
