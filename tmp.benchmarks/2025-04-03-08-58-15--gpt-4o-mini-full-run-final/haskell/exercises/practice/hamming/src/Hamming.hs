module Hamming (distance) where

import Data.Maybe (isNothing)

distance :: String -> String -> Maybe Int
distance xs ys
    | length xs /= length ys = Nothing
    | otherwise = Just $ length $ filter id $ zipWith (/=) xs ys
