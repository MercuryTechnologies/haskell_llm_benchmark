module BookStore (total, Book(..)) where

import Data.List (group, sort)

data Book = First | Second | Third | Fourth | Fifth
  deriving (Eq, Ord, Show)

-- | Calculate the total price in cents for a basket of books
total :: [Book] -> Int
total basket = minimum [calculatePrice grouping | grouping <- possibleGroupings]
  where
    bookCounts = map length $ group $ sort basket
    possibleGroupings = generateGroupings bookCounts

-- | Generate all possible ways to group books
generateGroupings :: [Int] -> [[[Int]]]
generateGroupings [] = [[]]
generateGroupings [n] = [[[n]]]
generateGroupings counts = concatMap (generateGroupingsWithLargest counts) [1..maximum counts]

-- | Generate groupings with a specific largest group size
generateGroupingsWithLargest :: [Int] -> Int -> [[[Int]]]
generateGroupingsWithLargest counts largestSize = 
  let (used, remaining) = extractGroups counts largestSize
  in if null used
     then []
     else map (\g -> used ++ g) (generateGroupings remaining)

-- | Extract groups of a specific size from the counts
extractGroups :: [Int] -> Int -> ([[Int]], [Int])
extractGroups counts size =
  let maxGroups = length $ filter (>= size) counts
      used = replicate maxGroups size
      remaining = zipWith subtract (map (\n -> min n size) counts) counts ++ 
                  drop (length counts) counts
  in (if null used then [] else [used], filter (> 0) remaining)

-- | Calculate the price for a specific grouping
calculatePrice :: [[Int]] -> Int
calculatePrice groups = sum $ map groupPrice groups
  where
    groupPrice :: [Int] -> Int
    groupPrice group = sum $ map bookSetPrice group
    
    bookSetPrice :: Int -> Int
    bookSetPrice n = n * discountedPrice n
    
    discountedPrice :: Int -> Int
    discountedPrice 1 = 800  -- 1 book: $8.00 = 800 cents
    discountedPrice 2 = 760  -- 2 books: $8.00 * 0.95 = $7.60 = 760 cents
    discountedPrice 3 = 720  -- 3 books: $8.00 * 0.90 = $7.20 = 720 cents
    discountedPrice 4 = 640  -- 4 books: $8.00 * 0.80 = $6.40 = 640 cents
    discountedPrice 5 = 600  -- 5 books: $8.00 * 0.75 = $6.00 = 600 cents
    discountedPrice _ = 800  -- Default case
