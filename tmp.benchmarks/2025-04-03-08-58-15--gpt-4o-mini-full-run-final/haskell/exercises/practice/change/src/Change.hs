module Change (findFewestCoins) where

import Data.List (sort)

findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins target coins
    | target < 0 = Nothing
    | target == 0 = Just []
    | otherwise = findCoins (sort (reverse coins)) target []

findCoins :: [Integer] -> Integer -> [Integer] -> Maybe [Integer]
findCoins _ 0 acc = Just acc
findCoins [] _ _ = Nothing
findCoins (c:cs) target acc
    | target >= c = case findCoins (c:cs) (target - c) (c:acc) of
        Just result -> Just result
        Nothing -> findCoins cs target acc
    | otherwise = findCoins cs target acc
