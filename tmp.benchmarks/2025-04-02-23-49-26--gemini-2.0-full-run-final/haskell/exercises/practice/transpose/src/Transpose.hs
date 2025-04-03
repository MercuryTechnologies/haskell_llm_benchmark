module Transpose (transpose) where

import Data.List (transpose)

transpose :: [String] -> [String]
transpose lines =
  let maxLength = maximum (map length lines)
      paddedLines = map (\line -> line ++ replicate (maxLength - length line) ' ') lines
  in Data.List.transpose paddedLines
