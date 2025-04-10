module Transpose (transpose) where

import Data.List hiding (transpose)

transpose :: [String] -> [String]
transpose = transpose . padToSameLength

padToSameLength :: [String] -> [String]
padToSameLength strs = map padToMaxLength strs
  where
    maxLen = maximum $ map length strs
    padToMaxLength str = replicate (maxLen - length str) ' ' ++ str
