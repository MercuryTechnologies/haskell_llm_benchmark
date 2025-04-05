module Proverb(recite) where

recite :: [String] -> String
recite [] = ""
recite items = unlines (zipWith line items (tail items)) ++ finalLine
  where
    line a b = "For want of a " ++ a ++ " the " ++ b ++ " was lost."
    finalLine = "And all for the want of a " ++ head items ++ "."
