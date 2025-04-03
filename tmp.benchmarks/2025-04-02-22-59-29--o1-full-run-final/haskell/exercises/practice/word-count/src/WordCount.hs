module WordCount (wordCount) where

import Data.Char (toLower, isAlphaNum)
import Data.List (dropWhileEnd, group, sort)

wordCount :: String -> [(String, Int)]
wordCount xs =
    let
        lowered = map toLower xs
        replaced = map (\c -> if isAlphaNum c || c == '\'' then c else ' ') lowered
        splitted = words replaced
        stripped = map stripApos splitted
        final = filter (not . null) stripped
        sortedWords = sort final
        grouped = group sortedWords
    in map (\g -> (head g, length g)) grouped

stripApos :: String -> String
stripApos = dropWhile (== '\'') . dropWhileEnd (== '\'')
