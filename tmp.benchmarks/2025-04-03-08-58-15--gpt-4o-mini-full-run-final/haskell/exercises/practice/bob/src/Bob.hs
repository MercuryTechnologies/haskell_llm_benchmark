module Bob (responseFor) where

import Data.Char (toUpper, isAlpha)

responseFor :: String -> String
responseFor xs
    | all isSpace xs = "Fine. Be that way!"
    | isYellingQuestion xs = "Calm down, I know what I'm doing!"
    | isYelling xs = "Whoa, chill out!"
    | isQuestion xs = "Sure."
    | otherwise = "Whatever."

isYelling :: String -> Bool
isYelling xs = xs == map toUpper xs && any isAlpha xs

isYellingQuestion :: String -> Bool
isYellingQuestion xs = isYelling xs && last xs == '?'

isQuestion :: String -> Bool
isQuestion xs = last xs == '?'

isSpace :: Char -> Bool
isSpace c = c == ' ' || c == '\t' || c == '\n'
