module Atbash (decode, encode) where

import qualified Data.Text as T
import           Data.Text (Text)

alphabet :: String
alphabet = ['a'..'z']

reverseAlphabet :: String
reverseAlphabet = reverse alphabet

encode :: Text -> Text
encode plainText = T.intercalate " " . map (T.pack . format) . groupByFive . T.unpack $ T.toLower plainText
  where
    format = map (\c -> if c `elem` alphabet then reverseAlphabet !! (fromEnum c - fromEnum 'a') else c)
    groupByFive [] = []
    groupByFive xs = take 5 xs : groupByFive (drop 5 xs)

decode :: Text -> Text
decode cipherText = T.pack . concat . map (map (\c -> if c `elem` reverseAlphabet then alphabet !! (fromEnum c - fromEnum 'z') else c)) . words $ T.unpack cipherText
