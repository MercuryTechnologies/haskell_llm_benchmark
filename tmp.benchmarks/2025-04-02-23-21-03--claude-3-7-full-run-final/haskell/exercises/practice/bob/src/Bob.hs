module Bob (responseFor) where

import Data.Char (isUpper, isLower, isAlpha, isSpace)

responseFor :: String -> String
responseFor xs
  | isSilence trimmed     = "Fine. Be that way!"
  | isYelling trimmed && isQuestion trimmed = "Calm down, I know what I'm doing!"
  | isYelling trimmed     = "Whoa, chill out!"
  | isQuestion trimmed    = "Sure."
  | otherwise             = "Whatever."
  where
    trimmed = trim xs
    
    -- Check if the input is a question (ends with '?')
    isQuestion :: String -> Bool
    isQuestion s = not (null s) && last s == '?'
    
    -- Check if the input is yelling (has at least one letter, all letters are uppercase)
    isYelling :: String -> Bool
    isYelling s = hasLetters s && not (any isLower (filter isAlpha s))
    
    -- Check if the input has any letters
    hasLetters :: String -> Bool
    hasLetters = any isAlpha
    
    -- Check if the input is silence (empty or only whitespace)
    isSilence :: String -> Bool
    isSilence = null
    
    -- Trim whitespace from both ends of a string
    trim :: String -> String
    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
