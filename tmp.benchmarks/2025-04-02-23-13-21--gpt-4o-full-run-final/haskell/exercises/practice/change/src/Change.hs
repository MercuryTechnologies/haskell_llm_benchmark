module Change (findFewestCoins) where

import Data.List (minimumBy)
import Data.Maybe (isNothing)
import Data.Ord (comparing)

findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins target coins
  | target < 0 = Nothing
  | otherwise = dp !! fromIntegral target
  where
    dp = map findMinCoins [0..target]
    findMinCoins 0 = Just []
    findMinCoins amt = 
      let possibleCoins = filter (not . isNothing) 
            [ fmap (coin :) (dp !! fromIntegral (amt - coin)) | coin <- coins, amt >= coin ]
      in if null possibleCoins then Nothing else minimumBy (comparing (fmap length)) possibleCoins
