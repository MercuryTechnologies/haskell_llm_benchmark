module Bob (responseFor) where

import Data.Char (isUpper, isSpace)

responseFor :: String -> String
responseFor xs
  | isSilent xs = "Fine. Be that way!"
  | isYelling xs && isQuestion xs = "Calm down, I know what I'm doing!"
  | isYelling xs = "Whoa, chill out!"
  | isQuestion xs = "Sure."
  | otherwise = "Whatever."
  where
    isSilent = all isSpace
    isQuestion str = not (null str) && last (filter (not . isSpace) str) == '?'
    isYelling str = any isUpper str && all (not . isLower) (filter isLetter str)
    isLower c = c >= 'a' && c <= 'z'
    isLetter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
