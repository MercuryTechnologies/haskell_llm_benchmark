module Beer (song) where

import Data.List (intercalate)
import Data.Char (toUpper) -- Import toUpper explicitly

-- | Generates the entire "99 Bottles of Beer" song lyrics.
song :: String
song = intercalate "\n" $ map verse [99, 98 .. 0]

-- | Generates a single verse of the song for a given number of bottles.
verse :: Int -> String
verse n = line1 n ++ line2 n
  where
    -- First line of the verse.
    line1 :: Int -> String
    line1 num = capitalize (bottlesOfBeer num) ++ " on the wall, " ++ bottlesOfBeer num ++ ".\n"

    -- Second line of the verse.
    line2 :: Int -> String
    line2 num = action num ++ bottlesOfBeer (next num) ++ " on the wall.\n"

    -- Determines the number of bottles for the next verse.
    next :: Int -> Int
    next 0 = 99 -- After 0 bottles, we buy more.
    next k = k - 1 -- Otherwise, we take one down.

-- | Returns the string representation for a given number of bottles (e.g., "1 bottle", "n bottles", "no more bottles").
bottlesOfBeer :: Int -> String
bottlesOfBeer 0 = "no more bottles of beer"
bottlesOfBeer 1 = "1 bottle of beer"
bottlesOfBeer n = show n ++ " bottles of beer"

-- | Returns the action phrase for the second line of the verse.
action :: Int -> String
action 0 = "Go to the store and buy some more, "
action 1 = "Take it down and pass it around, "
action _ = "Take one down and pass it around, " -- Default action for n > 1

-- | Capitalizes the first letter of a string.
capitalize :: String -> String
capitalize [] = []
capitalize (c:cs) = toUpper c : cs
