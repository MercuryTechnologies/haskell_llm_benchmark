module Isogram (isIsogram) where

import qualified Data.Text as T
import           Data.Text (Text)

isIsogram :: Text -> Bool
isIsogram input = 
    let cleanedInput = T.filter (/= '-') . T.filter (/= ' ') . T.toLower $ input
        uniqueChars = T.toList cleanedInput
    in length uniqueChars == length (T.unpack cleanedInput)
