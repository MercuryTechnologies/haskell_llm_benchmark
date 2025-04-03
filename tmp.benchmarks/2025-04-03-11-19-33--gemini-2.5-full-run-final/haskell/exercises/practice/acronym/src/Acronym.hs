module Acronym (abbreviate) where

import Data.Char (isLetter, isSpace, isUpper, isLower, toUpper)
import Data.List (zip)

-- Processes a single word to extract acronym characters.
-- Takes the first letter, and any subsequent uppercase letter
-- that follows a lowercase letter.
-- Example: processWord "HyperText" == "HT"
-- Example: processWord "word" == "w"
-- Example: processWord "GNU" == "G"
processWord :: String -> String
processWord [] = []
processWord word@(firstChar:_) = -- Handles non-empty list
  firstChar : relevantChars
  where
    -- Create pairs of adjacent characters: (previous, current)
    -- Example: "HyperText" -> [('H','y'), ('y','p'), ..., ('r','T'), ...]
    pairs = zip word (tail word)
    -- Keep the 'current' character if it's uppercase and the 'previous' was lowercase
    relevantChars = [curr | (prev, curr) <- pairs, isLower prev && isUpper curr]

-- | Converts a phrase to its acronym.
-- Hyphens are treated as word separators like spaces.
-- All other punctuation is removed.
-- CamelCase words contribute multiple letters (e.g., "HyperText" -> "HT").
-- The first letter of each resulting word/sub-word is capitalized and concatenated.
--
-- Examples:
--   abbreviate "Portable Network Graphics" == "PNG"
--   abbreviate "Liquid-crystal display" == "LCD"
--   abbreviate "Thank George It's Friday!" == "TGIF"
--   abbreviate "HyperText Markup Language" == "HTML"
abbreviate :: String -> String
abbreviate xs = map toUpper $ concatMap processWord ws
  where
    -- 1a. Replace hyphens with spaces
    replaceHyphen c = if c == '-' then ' ' else c
    hyphenated = map replaceHyphen xs
    -- 1b. Keep only letters and spaces (remove other punctuation)
    filtered = filter (\c -> isLetter c || isSpace c) hyphenated
    -- 2. Split into words based on spaces
    ws = words filtered
    -- 3. Apply processWord to each word and concatenate results
    -- 4. Uppercase the final string (handled in the main expression)

