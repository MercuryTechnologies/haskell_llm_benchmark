module Diamond (diamond) where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Char (ord, chr)

diamond :: Char -> Maybe [Text]
diamond c
  | c < 'A' || c > 'Z' = Nothing
  | otherwise = Just $ generateDiamond c

generateDiamond :: Char -> [Text]
generateDiamond c = top ++ bottom
  where
    n = ord c - ord 'A'
    top = map generateRow [0..n]
    bottom = reverse (init top)

    generateRow :: Int -> Text
    generateRow i =
      let
        letter = chr (ord 'A' + i)
        padding = T.replicate (n - i) (T.pack " ")
        innerPadding = T.replicate (2 * i - 1) (T.pack " ")
      in
        if i == 0
          then padding <> T.pack [letter] <> padding
          else padding <> T.pack [letter] <> innerPadding <> T.pack [letter] <> padding
