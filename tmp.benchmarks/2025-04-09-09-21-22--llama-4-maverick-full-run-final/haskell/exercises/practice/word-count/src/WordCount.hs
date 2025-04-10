module WordCount (wordCount) where

import Data.Char (isPunctuation, toLower)
import qualified Data.Map as M

wordCount :: String -> [(String, Int)]
wordCount xs = M.toList (countWords (cleanAndSplit xs))

cleanAndSplit :: String -> [String]
cleanAndSplit = words . map cleanChar
  where
    cleanChar c | c == '\'' = c
                | isPunctuation c = ' '
                | otherwise = toLower c

countWords :: [String] -> M.Map String Int
countWords = foldl' updateCount M.empty
  where
    updateCount m w = M.insertWith (+) w 1 m
