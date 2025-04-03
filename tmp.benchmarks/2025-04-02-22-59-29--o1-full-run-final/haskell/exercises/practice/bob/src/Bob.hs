module Bob (responseFor) where

import Data.Char (isAlpha, isSpace, isUpper)
import Data.List (dropWhileEnd)

responseFor :: String -> String
responseFor xs
  | isSilent                = "Fine. Be that way!"
  | isShoutedQuestion       = "Calm down, I know what I'm doing!"
  | isShouted               = "Whoa, chill out!"
  | isQuestion              = "Sure."
  | otherwise               = "Whatever."
  where
    trimmed = dropWhileEnd isSpace (dropWhile isSpace xs)
    isSilent = null trimmed
    isShoutedQuestion = isShouted && isQuestion
    isShouted = any isAlpha xs && all isUpper (filter isAlpha xs)
    isQuestion = not (null trimmed) && last trimmed == '?'
