module Isogram (isIsogram) where

import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Char (isLetter, toLower)
import           Data.List (nub)

isIsogram :: Text -> Bool
isIsogram text = 
    let letters = T.filter isLetter $ T.toLower text
    in T.length letters == length (nub $ T.unpack letters)
