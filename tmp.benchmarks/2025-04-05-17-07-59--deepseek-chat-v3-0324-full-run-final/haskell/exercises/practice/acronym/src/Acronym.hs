module Acronym (abbreviate) where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Char (isUpper, isAlpha, toUpper)

abbreviate :: Text -> Text
abbreviate = T.concat . map (T.take 1) . filter (not . T.null) . T.words . T.map replacePunctuation
  where
    replacePunctuation c
      | c == '-' = ' '
      | isAlpha c = toUpper c
      | otherwise = ' '
