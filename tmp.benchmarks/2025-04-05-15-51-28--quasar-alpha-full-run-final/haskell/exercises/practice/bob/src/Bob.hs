module Bob (responseFor) where

import Data.Char (isSpace, isAlpha, toUpper, isUpper)

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

isQuestion :: String -> Bool
isQuestion xs = not (null xs) && last xs == '?'

isYelling :: String -> Bool
isYelling xs = any isAlpha xs && all (not . isAlphaLower) xs
  where
    isAlphaLower c = isAlpha c && not (isUpper c)

responseFor :: String -> String
responseFor input
  | null trimmed = "Fine. Be that way!"
  | isYelling trimmed && isQuestion trimmed = "Calm down, I know what I'm doing!"
  | isYelling trimmed = "Whoa, chill out!"
  | isQuestion trimmed = "Sure."
  | otherwise = "Whatever."
  where
    trimmed = trim input
