module Series (slices) where

import Data.Char (digitToInt)
import Data.List (tails, replicate)

slices :: Int -> String -> [[Int]]
slices n xs
    -- If the requested slice length is exactly 0, return L+1 empty lists.
    | n == 0    = replicate (length xs + 1) []
    -- If the requested slice length is negative, return an empty list.
    | n < 0     = []
    -- Otherwise (n > 0), proceed with generating slices.
    | otherwise =
        -- 1. Generate all tails (suffixes) of the input string xs.
        --    Example: tails "49142" -> ["49142", "9142", "142", "42", "2", ""]
        let allTails = tails xs
        -- 2. Determine how many tails are long enough to provide a slice of length n.
        --    This is equal to the number of possible start positions for a slice.
        --    Example: n=3, xs="49142" (len=5) -> 5 - 3 + 1 = 3 possible slices.
            numberOfSlices = length xs - n + 1
        -- 3. Take only the tails that can actually produce a slice of length n.
        --    If n > length xs, numberOfSlices will be <= 0, and 'take' will correctly produce [].
        --    Example: take 3 ["49142", "9142", "142", "42", "2", ""] -> ["49142", "9142", "142"]
            relevantTails = take numberOfSlices allTails
        -- 4. For each relevant tail, take the first n characters to form the substring.
        --    Example: map (take 3) ["49142", "9142", "142"] -> ["491", "914", "142"]
            substrings = map (take n) relevantTails
        -- 5. For each substring, convert its characters to integers.
        --    Example: map (map digitToInt) ["491", "914", "142"] -> [[4,9,1], [9,1,4], [1,4,2]]
         in map (map digitToInt) substrings

-- More compact version of the 'otherwise' branch:
-- map (map digitToInt . take n) (take (length xs - n + 1) (tails xs))
