module Bob (responseFor) where

import Data.Char (isUpper, isLetter, isSpace)
import Data.List (dropWhileEnd) -- Import dropWhileEnd for trimming

-- Helper function to trim trailing whitespace
trimEnd :: String -> String
trimEnd = dropWhileEnd isSpace

-- Helper function to check if a string consists only of whitespace or is empty
isSilence :: String -> Bool
isSilence = all isSpace

-- Helper function to check if a string is yelled (all letters are uppercase)
-- It must contain at least one letter.
isYelling :: String -> Bool
isYelling xs = any isLetter xs && not (any (\c -> isLetter c && not (isUpper c)) xs)

-- Helper function to check if a string is a question (ends with '?')
isQuestion :: String -> Bool
isQuestion xs = case trimEnd xs of -- Trim trailing whitespace before checking
                  [] -> False      -- Empty string is not a question
                  trimmed -> last trimmed == '?'

responseFor :: String -> String
responseFor phrase
  | isSilence phrase = "Fine. Be that way!"
  | isYelling phrase && isQuestion phrase = "Calm down, I know what I'm doing!"
  | isYelling phrase = "Whoa, chill out!"
  | isQuestion phrase = "Sure."
  | otherwise = "Whatever."

