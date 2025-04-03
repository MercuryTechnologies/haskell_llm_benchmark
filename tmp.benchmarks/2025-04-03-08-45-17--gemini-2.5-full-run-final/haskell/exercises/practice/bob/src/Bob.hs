module Bob (responseFor) where

import Data.Char (isSpace, isUpper, isLetter)
import Data.List (dropWhileEnd)

-- Helper function to check if a string consists only of whitespace.
isSilence :: String -> Bool
isSilence = all isSpace

-- Helper function to check if a string is yelled (has letters, and all letters are uppercase).
isYelling :: String -> Bool
isYelling xs = hasLetters && allUpper
  where
    letters = filter isLetter xs
    hasLetters = not (null letters)
    allUpper = all isUpper letters

-- Helper function to check if a string is a question (ends with '?' after trimming trailing whitespace).
isQuestion :: String -> Bool
isQuestion xs = case dropWhileEnd isSpace xs of
                  "" -> False -- Empty or whitespace-only string is not a question
                  ys -> last ys == '?'

-- Determine Bob's response based on the input string.
responseFor :: String -> String
responseFor xs
    | isSilence xs             = "Fine. Be that way!"
    | isYelling xs && isQuestion xs = "Calm down, I know what I'm doing!"
    | isYelling xs            = "Whoa, chill out!"
    | isQuestion xs            = "Sure."
    | otherwise                = "Whatever."
