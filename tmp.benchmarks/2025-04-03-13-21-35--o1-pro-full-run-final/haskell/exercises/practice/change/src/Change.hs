module Change (findFewestCoins) where

import Data.List (minimumBy, sort)
import Data.Function (on)

findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins target coins
  | target < 0  = Nothing
  | target == 0 = Just []
  | otherwise   =
      let sortedCoins = sort coins  -- Sort the coin list
          -- Build a list dp indexed by amounts from 0 to target
          -- dp !! i will hold Maybe [Integer], the best combination for amount i
          dp = map best [0..target]
          best 0 = Just []
          best amt =
            let possibilities =
                  [ coin : combo
                  | coin <- sortedCoins
                  , coin <= amt
                  , Just combo <- [dp !! fromInteger (amt - coin)]
                  ]
            in if null possibilities
               then Nothing
               else Just (minimumBy (compare `on` length) possibilities)
      in dp !! fromInteger target
