module Bob (responseFor) where

import qualified Data.Text as T
import           Data.Text (Text)

responseFor :: Text -> Text
responseFor xs
  | isSilence xs = "Fine. Be that way!"
  | isYellingQuestion xs = "Calm down, I know what I'm doing!"
  | isYelling xs = "Whoa, chill out!"
  | isQuestion xs = "Sure."
  | otherwise = "Whatever."

isSilence :: Text -> Bool
isSilence xs = T.strip xs == ""

isYelling :: Text -> Bool
isYelling xs = T.toUpper xs == xs && T.toLower xs /= xs

isQuestion :: Text -> Bool
isQuestion xs = T.strip xs `T.isSuffixOf` "?"

isYellingQuestion :: Text -> Bool
isYellingQuestion xs = isYelling xs && isQuestion xs
