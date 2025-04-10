module Proverb(recite) where

recite :: [String] -> String
recite [] = ""
recite xs = unlines (map verse (zip xs (tail xs))) ++ finalVerse (head xs)

verse :: (String, String) -> String
verse (x, y) = "For want of a " ++ x ++ " the " ++ y ++ " was lost."

finalVerse :: String -> String
finalVerse x = "And all for the want of a " ++ x ++ "."
