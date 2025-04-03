module Proverb(recite) where

recite :: [String] -> String
recite [] = ""
recite xs = concatMap line (zip xs (tail xs)) ++ [lastLine [head xs]]
  where
    line (x, y) = "For want of a " ++ x ++ " the " ++ y ++ " was lost.\n"
    lastLine x   = "And all for the want of a " ++ x ++ "."
