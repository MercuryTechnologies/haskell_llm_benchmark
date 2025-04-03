module Change (findFewestCoins) where

import Data.List (sortOn)

findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins target coins
  | target < 0 = Nothing            -- Negative change is not possible
  | target == 0 = Just []           -- Zero change means no coins
  | null validCoins = Nothing       -- No valid coins
  | otherwise = memo !! fromInteger target
  where
    validCoins = filter (> 0) coins  -- Only positive coins
    
    -- Memoization table
    memo = map dpCalc [0..fromInteger target]
    
    -- DP calculation function
    dpCalc :: Int -> Maybe [Integer]
    dpCalc 0 = Just []
    dpCalc amt = 
      case options of
        [] -> Nothing
        _  -> Just $ head $ sortByLengthAndValue options
      where
        options = [coin : rest | 
                  coin <- validCoins, 
                  fromInteger coin <= amt, 
                  Just rest <- [memo !! (amt - fromInteger coin)]]
        
        -- Sort first by length, then by coin values
        sortByLengthAndValue :: [[Integer]] -> [[Integer]]
        sortByLengthAndValue = sortOn (\xs -> (length xs, xs))
