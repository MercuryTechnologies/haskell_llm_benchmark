module Bob (responseFor) where

import Data.Char (isSpace, isAlpha, isUpper, isLower)

responseFor :: String -> String
responseFor xs
  | isSilence xs = "Fine. Be that way!"
  | isYellQuestion xs = "Calm down, I know what I'm doing!"
  | isYell xs = "Whoa, chill out!"
  | isQuestion xs = "Sure."
  | otherwise = "Whatever."

isSilence :: String -> Bool
isSilence = all isSpace

isQuestion :: String -> Bool
isQuestion s = case dropWhileEnd isSpace s of
  [] -> False
  trimmed -> last trimmed == '?'

isYell :: String -> Bool
isYell s = let letters = filter isAlpha s
           in not (null letters) && all isUpper letters

isYellQuestion :: String -> Bool
isYellQuestion s = isYell s && isQuestion s

-- dropWhileEnd is only in Data.List since base-4.5.0.0, so define it for compatibility
dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p = reverse . dropWhile p . reverse
