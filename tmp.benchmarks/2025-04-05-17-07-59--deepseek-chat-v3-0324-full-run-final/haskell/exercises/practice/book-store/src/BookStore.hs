module BookStore (total, Book(..)) where

import Data.List (group, sort)

data Book = First | Second | Third | Fourth | Fifth
    deriving (Eq, Show, Enum, Bounded)

bookPrice :: Int
bookPrice = 800  -- price in cents

discounts :: [Double]
discounts = [0, 0, 0.05, 0.10, 0.20, 0.25]  -- index represents group size

total :: [Book] -> Int
total basket = minimum $ map calculatePrice possibleGroupings
  where
    counts = map length $ group $ sort basket
    possibleGroupings = generateGroupings counts
    
    calculatePrice groups = round $ sum $ map groupPrice groups
    groupPrice groupSize = fromIntegral groupSize * fromIntegral bookPrice * (1 - discounts !! groupSize)

    -- Generate all possible groupings by trying different splits of 5+3 vs 4+4
    generateGroupings :: [Int] -> [[Int]]
    generateGroupings counts = 
        let maxGroupSize = min 5 (length counts)
            groups = take maxGroupSize counts
            remaining = drop maxGroupSize counts
            nextCounts = zipWith subtract (map (const 1) groups) groups ++ remaining
            nextCounts' = filter (>0) nextCounts
        in if null counts
            then [[]]
            else [maxGroupSize : g | g <- generateGroupings nextCounts'] ++
                 if maxGroupSize >= 4
                    then let altGroupSize = maxGroupSize - 1
                             altNextCounts = zipWith subtract (map (const 1) (take altGroupSize groups)) groups ++ remaining
                             altNextCounts' = filter (>0) altNextCounts
                         in [altGroupSize : g | g <- generateGroupings altNextCounts']
                    else []
