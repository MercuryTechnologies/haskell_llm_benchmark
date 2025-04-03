module Diamond (diamond) where

import Data.Char (isUpper, ord)

diamond :: Char -> Maybe [String]
diamond ch
  | not (isUpper ch) = Nothing
  | otherwise = Just $ makeDiamond ch

makeDiamond :: Char -> [String]
makeDiamond ch = topHalf ++ tail (reverse topHalf)
  where
    -- Generate rows for letters from 'A' to ch
    topHalf = [makeRow c ch | c <- ['A'..ch]]

makeRow :: Char -> Char -> String
makeRow row maxChar = 
  let
    -- Calculate position information
    width = 2 * (ord maxChar - ord 'A') + 1
    rowPos = ord row - ord 'A'
    outerSpaces = (width - (2 * rowPos + 1)) `div` 2
  in
    if row == 'A'
    then replicate outerSpaces ' ' ++ "A" ++ replicate outerSpaces ' '
    else
      let innerSpaces = 2 * rowPos - 1
      in replicate outerSpaces ' ' ++ [row] ++ replicate innerSpaces ' ' ++ [row] ++ replicate outerSpaces ' '
