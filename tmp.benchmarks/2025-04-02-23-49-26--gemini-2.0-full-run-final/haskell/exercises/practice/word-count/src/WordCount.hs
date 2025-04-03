module WordCount (wordCount) where

import Data.Char (toLower, isAlphaNum)
import Data.List (group, sort)

wordCount :: String -> [(String, Int)]
wordCount text = wordCounts
  where
    normalizedText = map toLower text
    words = splitIntoWords normalizedText
    wordCounts = map (\xs -> (head xs, length xs)) (group (sort words))

splitIntoWords :: String -> [String]
splitIntoWords text = filter (not . null) $ words' text
  where
    words' "" = []
    words' xs =
      let (word, rest) = extractWord xs
       in word : words' rest

extractWord :: String -> (String, String)
extractWord "" = ("", "")
extractWord xs =
  case span isWordChar xs of
    ("", ys) -> extractWord (dropWhile (not . isWordChar) ys)
    (word, ys) -> (word, dropWhile (not . isWordChar) ys)

isWordChar :: Char -> Bool
isWordChar c = isAlphaNum c || c == '\''

-- Modified isWordChar and extractWord to handle quotes correctly
-- isWordChar c = isAlphaNum c
-- extractWord "" = ("", "")
-- extractWord xs =
--   let (firstChar:rest) = xs
--   in if firstChar == '\''
--     then
--       let (word, rest') = extractWord' rest
--       in ("'" ++ word, rest')
--     else
--       let (word, rest') = extractWord' xs
--       in (word, rest')

-- extractWord' "" = ("", "")
-- extractWord' xs =
--   case span isWordChar' xs of
--     ("", ys) -> ("", ys)
--     (word, ys) -> (word, ys)

-- isWordChar' :: Char -> Bool
-- isWordChar' c = isAlphaNum c || c == '\''
