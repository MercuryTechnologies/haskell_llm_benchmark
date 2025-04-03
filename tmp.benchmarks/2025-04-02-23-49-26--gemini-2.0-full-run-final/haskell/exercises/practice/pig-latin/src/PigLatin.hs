module PigLatin (translate) where

import qualified Data.Text as T
import Data.Text (Text)

translate :: String -> String
translate = T.unpack . translateText . T.pack

translateText :: Text -> Text
translateText word =
  case T.uncons word of
    Just (first, rest)
      | first `elem` vowels -> word <> "ay"
      | T.isPrefixOf "xr" word -> word <> "ay"
      | T.isPrefixOf "yt" word -> word <> "ay"
      | T.isPrefixOf "qu" word -> T.drop 2 word <> "quay"
      | otherwise ->
          case T.findIndex (`elem` vowels) word of
            Just vowelIndex ->
              if T.take 2 (T.drop vowelIndex word) == "qu"
                then T.drop (vowelIndex + 2) word <> T.take (vowelIndex + 2) word <> "ay"
                else T.drop vowelIndex word <> T.take vowelIndex word <> "ay"
            Nothing ->
              case T.findIndex (== 'y') word of
                Just yIndex -> T.drop (yIndex) word <> T.take yIndex word <> "ay"
                Nothing -> word <> "ay" -- If no vowels, add "ay" at the end. This might not be correct, but handles cases with no vowels.
    Nothing -> ""

vowels :: [Char]
vowels = ['a', 'e', 'i', 'o', 'u']
