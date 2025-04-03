module Diamond (diamond) where

import Data.Char (ord, chr)

diamond :: Char -> Maybe [String]
diamond c
  | c < 'A' || c > 'Z' = Nothing
  | otherwise          = Just $ map (buildRow n) [0 .. 2*n]
  where
    n = ord c - ord 'A'

buildRow :: Int -> Int -> String
buildRow n i =
  let letterIndex = if i <= n then i else 2*n - i
      letter      = chr (ord 'A' + letterIndex)
      leading     = replicate (n - letterIndex) ' '
  in
    if letterIndex == 0
      then leading ++ [letter] ++ leading
      else
        let middle = replicate (2 * letterIndex - 1) ' '
        in leading ++ [letter] ++ middle ++ [letter] ++ leading
