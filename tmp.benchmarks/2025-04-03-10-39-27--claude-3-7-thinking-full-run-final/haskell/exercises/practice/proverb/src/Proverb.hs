module Proverb(recite) where

recite :: [String] -> String
recite [] = ""
recite [x] = "And all for the want of a " ++ x ++ "."
recite (x:xs) = 
    unlines (regularLines ++ [lastLine])
  where
    regularLines = zipWith makeLine (x:xs) xs
    lastLine = "And all for the want of a " ++ x ++ "."
    makeLine a b = "For want of a " ++ a ++ " the " ++ b ++ " was lost."
