module Transpose (transpose) where

transpose :: [String] -> [String]
transpose lines = map (map replaceNull) $ foldr (zipWith (:) . padTo maxLength) (repeat []) lines
  where
    maxLength = maximum (map length lines)
    padTo n xs = xs ++ replicate (n - length xs) ' '
    replaceNull c = if c == '\0' then ' ' else c
