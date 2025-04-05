module Acronym (abbreviate) where

import Data.Char (isAlpha, isUpper, toUpper)

abbreviate :: String -> String
abbreviate xs = map (toUpper . head) (words cleaned ++ camelWords)
  where
    -- Replace hyphens with spaces, remove other punctuation
    cleaned = map replaceOrKeep xs
    replaceOrKeep c
      | c == '-'        = ' '
      | isAlpha c || c == ' ' = c
      | otherwise       = ' '

    -- Extract uppercase letters inside words (camel case)
    camelWords = concatMap extractCamel (words cleaned)

    extractCamel [] = []
    extractCamel (c:cs) = [c | isUpper c] ++ extractCamel cs
