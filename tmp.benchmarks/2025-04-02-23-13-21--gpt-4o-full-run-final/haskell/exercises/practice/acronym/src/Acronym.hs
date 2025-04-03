module Acronym (abbreviate) where

import qualified Data.Text as T
import           Data.Text (Text)

abbreviate :: Text -> Text
abbreviate xs = T.concat $ map (T.take 1) $ T.words $ T.map replaceHyphen $ T.filter isAllowed xs
  where
    replaceHyphen '-' = ' '
    replaceHyphen c   = c
    isAllowed c = T.isAlpha c || c == '-'
