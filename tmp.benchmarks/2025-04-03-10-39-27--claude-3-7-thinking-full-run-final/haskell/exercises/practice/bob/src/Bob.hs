module Bob (responseFor) where

import Data.Char (isAlpha, isSpace, isUpper)

responseFor :: String -> String
responseFor xs
  | isSilence xs                  = "Fine. Be that way!"
  | isYelling xs && isQuestion xs = "Calm down, I know what I'm doing!"
  | isYelling xs                  = "Whoa, chill out!"
  | isQuestion xs                 = "Sure."
  | otherwise                     = "Whatever."

isSilence :: String -> Bool
isSilence = all isSpace

isYelling :: String -> Bool
isYelling xs = any isAlpha xs && all isUpper (filter isAlpha xs)

isQuestion :: String -> Bool
isQuestion xs = case dropWhile isSpace (reverse xs) of
                  '?':_ -> True
                  _     -> False
