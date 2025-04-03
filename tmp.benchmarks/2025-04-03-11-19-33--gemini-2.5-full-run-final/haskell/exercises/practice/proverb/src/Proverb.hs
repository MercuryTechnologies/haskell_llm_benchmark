module Proverb(recite) where

import Data.List (intercalate) -- Import intercalate for joining lines

-- | Given a list of strings, generates the proverb.
recite :: [String] -> String
recite [] = "" -- If the list is empty, return an empty string.
recite ws@(first:_) = -- Use pattern matching and an @-pattern to get the head and the full list.
    let
        -- Create pairs of consecutive items from the list.
        -- e.g., ["nail", "shoe", "horse"] -> [("nail", "shoe"), ("shoe", "horse")]
        pairs = zip ws (tail ws)

        -- Format each pair into a line of the proverb.
        -- e.g., ("nail", "shoe") -> "For want of a nail the shoe was lost."
        pairLines = map formatPairLine pairs
          where formatPairLine (want, lost) = "For want of a " ++ want ++ " the " ++ lost ++ " was lost."

        -- Format the final line using the first item of the original list.
        -- e.g., "nail" -> "And all for the want of a nail."
        finalLine = "And all for the want of a " ++ first ++ "."

    -- Combine the generated lines for pairs and the final line, separated by newlines.
    -- intercalate ensures no trailing newline, matching the example output.
    in intercalate "\n" (pairLines ++ [finalLine])
