module Bob (responseFor) where

import Data.Char (isAlpha, isSpace, isUpper)
import Data.List (dropWhileEnd)

responseFor :: String -> String
responseFor xs
  | isSilent trimmed = "Fine. Be that way!"
  | isYell trimmed && isQuestion trimmed = "Calm down, I know what I'm doing!"
  | isYell trimmed = "Whoa, chill out!"
  | isQuestion trimmed = "Sure."
  | otherwise = "Whatever."
  where
    trimmed = trim xs

isSilent :: String -> Bool
isSilent s = null s

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

isQuestion :: String -> Bool
isQuestion "" = False
isQuestion s = last s == '?'

isYell :: String -> Bool
isYell s =
  let letters = filter isAlpha s
  in not (null letters) && all isUpper letters
