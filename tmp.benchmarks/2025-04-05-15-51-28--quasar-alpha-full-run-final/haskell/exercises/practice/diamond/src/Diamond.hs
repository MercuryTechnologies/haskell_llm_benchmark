module Diamond (diamond) where

import Data.Char (ord, chr)

diamond :: Char -> Maybe [String]
diamond c
  | not (c >= 'A' && c <= 'Z') = Nothing
  | otherwise = Just $ map (makeLine maxIdx) [0..maxIdx] ++ tail (reverse $ map (makeLine maxIdx) [0..maxIdx])
  where
    maxIdx = ord c - ord 'A'

makeLine :: Int -> Int -> String
makeLine maxIdx idx =
  let letter = chr (ord 'A' + idx)
      outerSpaces = replicate (maxIdx - idx) ' '
      innerSpaces = if idx == 0 then "" else replicate (2 * idx - 1) ' '
  in if idx == 0
     then outerSpaces ++ [letter] ++ outerSpaces
     else outerSpaces ++ [letter] ++ innerSpaces ++ [letter] ++ outerSpaces
