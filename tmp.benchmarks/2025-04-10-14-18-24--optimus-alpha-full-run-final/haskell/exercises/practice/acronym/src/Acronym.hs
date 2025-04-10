module Acronym (abbreviate) where

import Data.Char (isAlpha, isUpper, toUpper)

abbreviate :: String -> String
abbreviate xs = map toUpper $ acronym xs
  where
    acronym [] = []
    acronym (c:cs)
      | isAlpha c =
          -- Start of a word or uppercase in camelCase
          if null cs || not (isAlpha (head cs))
            then [c] ++ acronym cs
            else
              if c == '-' then acronym cs
              else
                -- If at start or previous is not alpha, or previous is lowercase and c is uppercase (camelCase)
                if isWordStart c cs
                  then [c] ++ acronym cs
                  else acronym cs
      | c == '-' = ' ' : acronym cs
      | otherwise = acronym cs

    -- A word start is when previous char is not a letter, or previous is lowercase and current is uppercase
    isWordStart c cs =
      case cs of
        [] -> True
        (p:_) -> not (isAlpha p) || (isUpper c && not (isUpper p))

-- Improved version: split into words, handle camelCase, hyphens, and ignore other punctuation
abbreviate :: String -> String
abbreviate = map toUpper . extractInitials . preprocess
  where
    preprocess = map (\c -> if c == '-' then ' ' else if isAlpha c || c == ' ' then c else ' ')
    extractInitials = concatMap getInitial . words
    getInitial [] = []
    getInitial (x:xs) = x : [c | (p, c) <- zip (x:xs) xs, isUpper c && not (isUpper p)]
