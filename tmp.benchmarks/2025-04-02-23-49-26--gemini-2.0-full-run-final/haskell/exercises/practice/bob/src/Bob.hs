module Bob (responseFor) where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Char (isUpper, isSpace)

responseFor :: Text -> Text
responseFor xs
  | T.all isSpace xs = T.pack "Fine. Be that way!"
  | isQuestion && isYelling = T.pack "Calm down, I know what I'm doing!"
  | isYelling = T.pack "Whoa, chill out!"
  | isQuestion = T.pack "Sure."
  | otherwise = T.pack "Whatever."
  where
    isQuestion = T.last xs == '?'
    isYelling = not (T.any isLower alphaOnly) && T.any isAlpha alphaOnly
    alphaOnly = T.filter isAlpha xs
    isAlpha c = not (isSpace c) && ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z'))
    isLower c = c >= 'a' && c <= 'z'
