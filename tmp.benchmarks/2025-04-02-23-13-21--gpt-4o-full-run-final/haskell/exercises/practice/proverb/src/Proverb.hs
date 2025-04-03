module Proverb(recite) where

recite :: [String] -> String
recite [] = ""
recite [x] = "And all for the want of a " ++ x ++ "."
recite items = unlines (map createLine pairs ++ [finalLine])
  where
    pairs = zip items (tail items)
    createLine (a, b) = "For want of a " ++ a ++ " the " ++ b ++ " was lost."
    finalLine = "And all for the want of a " ++ head items ++ "."
