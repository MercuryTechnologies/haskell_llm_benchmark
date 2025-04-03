module Acronym (abbreviate) where

import qualified Data.Text as T
import           Data.Text (Text)

abbreviate :: Text -> Text
abbreviate xs = T.toUpper $ T.concatMap firstLetter words
  where
    words = T.splitOn (T.pack "-") (T.filter isValidChar xs)
    isValidChar c = T.isAlpha c || c == ' '
    firstLetter word = if T.null word then T.empty else T.take 1 word
