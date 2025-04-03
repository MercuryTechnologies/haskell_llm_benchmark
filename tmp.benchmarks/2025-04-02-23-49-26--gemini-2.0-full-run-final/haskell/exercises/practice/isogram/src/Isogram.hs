module Isogram (isIsogram) where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Char (toLower)
import Data.Set (Set, fromList)

isIsogram :: String -> Bool
isIsogram text =
  let
    lowerText :: Text
    lowerText = T.toLower $ T.pack text
    lettersOnly :: Text
    lettersOnly = T.filter T.isLetter lowerText
    letterList :: [Char]
    letterList = T.unpack lettersOnly
    letterSet :: Set Char
    letterSet = fromList letterList
  in
    length letterList == Data.Set.size letterSet
