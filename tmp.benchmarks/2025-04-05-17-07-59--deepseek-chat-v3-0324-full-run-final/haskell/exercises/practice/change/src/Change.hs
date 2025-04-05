module Change (findFewestCoins) where

import Data.List (sort, nub)

findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins target coins
    | target < 0  = Nothing
    | target == 0 = Just []
    | null validCoins = Nothing
    | otherwise = findChange target (reverse $ sort validCoins) []
  where
    validCoins = filter (>0) $ nub coins

    findChange remaining [] _ = Nothing
    findChange 0 _ acc = Just (reverse acc)
    findChange remaining (c:cs) acc
        | c > remaining = findChange remaining cs acc
        | otherwise = case findChange (remaining - c) (c:cs) (c:acc) of
            Just result -> Just result
            Nothing -> findChange remaining cs acc
