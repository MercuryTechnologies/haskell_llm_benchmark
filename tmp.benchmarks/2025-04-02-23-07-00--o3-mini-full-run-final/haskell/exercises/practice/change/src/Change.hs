module Change (findFewestCoins) where

import Data.List (minimumBy)
import Data.Ord (comparing)

findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins target coins
  | target < 0  = Nothing
  | target == 0 = Just []
  | otherwise   = dp !! fromIntegral target
  where
    t :: Int
    t = fromIntegral target

    dp :: [Maybe [Integer]]
    dp = map best [0..t]

    best :: Int -> Maybe [Integer]
    best 0 = Just []
    best amt =
      let possible = [ coin : comb
                     | coin <- coins
                     , let coinInt = fromIntegral coin :: Int
                     , coinInt <= amt
                     , let remainder = amt - coinInt
                     , Just comb <- [dp !! remainder]
                     ]
      in if null possible
            then Nothing
            else Just (minimumBy (comparing length) possible)
