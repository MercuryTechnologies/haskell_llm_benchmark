module Change (findFewestCoins) where

import Data.List (minimumBy)
import Data.Ord  (comparing)

findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins target coins
  | target < 0  = Nothing
  | target == 0 = Just []
  | otherwise   =
      let n = fromIntegral target
          dp = [ computeCoins i | i <- [0..n] ]
          computeCoins 0 = Just []
          computeCoins i =
            let i' = fromIntegral i
                candidates =
                  [ coin : combo
                  | coin <- coins
                  , i' - coin >= 0
                  , let prev = dp !! fromIntegral (i' - coin)
                  , Just combo <- [prev]
                  ]
            in case candidates of
                 [] -> Nothing
                 _  -> Just (minimumBy (comparing length) candidates)
      in dp !! n
