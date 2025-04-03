module Acronym (abbreviate) where

import qualified Data.Text as T
import Data.Text (Text)

abbreviate :: String -> String
abbreviate xs = T.unpack $ T.concat $ map firstLetter $ T.splitOn (T.pack " ") $ T.words $ T.map replacePunctuation $ T.pack xs
  where
    firstLetter word = T.toUpper $ T.take 1 word
    replacePunctuation c = if c `elem` ['.', ',', '!', '&', '@', '$', '%', '^', '*', '(', ')', '_', '+', '=', '`', '~', '[', ']', '{', '}', ';', ':', '"', '<', '>', '/', '\\', '|'] then ' ' else if c == '-' then ' ' else c
