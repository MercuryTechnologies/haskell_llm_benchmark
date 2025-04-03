module WordCount (wordCount) where

import Data.Char (isAlphaNum, toLower)
import Data.List (group, sort)

wordCount :: String -> [(String, Int)]
wordCount = map (\xs -> (head xs, length xs)) 
          . group 
          . sort 
          . map (map toLower) 
          . filter (not . null)
          . words'
  where
    -- Custom word splitting function that handles contractions
    words' :: String -> [String]
    words' [] = []
    words' s = case dropWhile isSeparator s of
                [] -> []
                s' -> w : words' s''
                  where (w, s'') = break isSeparator $ cleanWord s'
    
    -- A character is a separator if it's not alphanumeric and not an apostrophe
    -- within a word (contractions)
    isSeparator :: Char -> Bool
    isSeparator c = not (isAlphaNum c) && not (isApostropheInWord c)
    
    -- Check if an apostrophe is within a word (part of a contraction)
    isApostropheInWord :: Char -> Bool
    isApostropheInWord '\'' = True
    isApostropheInWord _ = False
    
    -- Clean a word by removing surrounding quotation marks
    cleanWord :: String -> String
    cleanWord s = filter (\c -> isAlphaNum c || isApostropheInWord c) s
