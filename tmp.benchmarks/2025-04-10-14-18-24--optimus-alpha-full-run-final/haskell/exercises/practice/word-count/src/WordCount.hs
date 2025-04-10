module WordCount (wordCount) where

import Data.Char (isAlphaNum, toLower)
import Data.List (group, sort)
import qualified Data.Map.Strict as Map

-- | Split a string into words, keeping apostrophes inside words (for contractions)
splitWords :: String -> [String]
splitWords = go []
  where
    go acc [] = reverse acc
    go acc s =
      let s' = dropWhile (not . isWordStart) s
      in case s' of
           [] -> reverse acc
           _  ->
             let (w, rest) = span isWordChar s'
             in go (w : acc) rest

    isWordStart c = isAlphaNum c || c == '\''
    isWordChar c = isAlphaNum c || c == '\''

    -- But we want to allow apostrophes only *inside* words, not at the start or end
    -- So after splitting, we filter out leading/trailing apostrophes
    cleanWord = dropWhile (== '\'') . reverse . dropWhile (== '\'') . reverse

-- | Normalize a word: lowercase, remove leading/trailing apostrophes
normalize :: String -> String
normalize = dropWhile (== '\'') . reverse . dropWhile (== '\'') . reverse . map toLower

wordCount :: String -> [(String, Int)]
wordCount xs =
  let wordsRaw = splitWords xs
      wordsNorm = map normalize wordsRaw
      wordsFiltered = filter (not . null) wordsNorm
      counts = Map.fromListWith (+) [(w, 1) | w <- wordsFiltered]
  in Map.toList counts
