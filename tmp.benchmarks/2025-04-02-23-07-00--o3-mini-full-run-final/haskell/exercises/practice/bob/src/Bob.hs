module Bob (responseFor) where

import Data.Char (isAlpha, isSpace, isUpper)

responseFor :: String -> String
responseFor s
  | isSilence s               = "Fine. Be that way!"
  | isQuestion ts && isShouting s = "Calm down, I know what I'm doing!"
  | isQuestion ts             = "Sure."
  | isShouting s              = "Whoa, chill out!"
  | otherwise                 = "Whatever."
  where
    ts = trim s

-- | Remove leading and trailing whitespace from a string.
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

-- | Check if a string is silence (empty or only whitespace).
isSilence :: String -> Bool
isSilence xs = all isSpace xs

-- | Check if a string is a question (trimmed string ends with a question mark).
isQuestion :: String -> Bool
isQuestion [] = False
isQuestion xs = last xs == '?'

-- | Check if a string is shouted:
-- There must be at least one alphabetical character and all alphabetical characters must be uppercase.
isShouting :: String -> Bool
isShouting xs =
  let letters = filter isAlpha xs
  in not (null letters) && all isUpper letters
