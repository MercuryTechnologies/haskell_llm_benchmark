module PigLatin (translate) where

import Data.Char (isAlpha)
import qualified Data.Text as T
import           Data.Text (Text)

translate :: Text -> Text
translate xs = T.unwords $ map translateWord (T.words xs)

translateWord :: Text -> Text
translateWord word
  | T.null word = word
  | T.head word `elem` "aeiou" || T.isPrefixOf "xr" word || T.isPrefixOf "yt" word = word `T.append` "ay"
  | T.isPrefixOf "qu" word = T.dropWhile isAlpha word `T.append` T.takeWhile isAlpha word `T.append` "ay"
  | T.last word == 'y' = T.dropWhile isAlpha word `T.append` T.takeWhile isAlpha word `T.append` "ay"
  | otherwise = let (prefix, rest) = T.span (not . (`elem` "aeiou")) word
                in rest `T.append` prefix `T.append` "ay"
