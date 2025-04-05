module WordCount (wordCount) where

import Data.Char (isAlphaNum, toLower)
import Data.List (group, sort)

wordCount :: String -> [(String, Int)]
wordCount xs = map (\ws -> (head ws, length ws)) . group . sort $ extractWords xs

extractWords :: String -> [String]
extractWords = map (map toLower) . filter (not . null) . splitWords
  where
    splitWords [] = []
    splitWords s =
      let (word, rest) = break (not . isWordChar) s
          rest' = dropWhile (not . isWordChar) rest
      in word : splitWords rest'

    isWordChar c = isAlphaNum c || c == '\''
