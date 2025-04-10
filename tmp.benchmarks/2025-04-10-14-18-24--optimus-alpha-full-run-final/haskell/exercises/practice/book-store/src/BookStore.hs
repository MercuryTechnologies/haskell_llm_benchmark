module BookStore (total, Book(..)) where

data Book = First | Second | Third | Fourth | Fifth
  deriving (Eq, Ord, Show)

import Data.List (sort, group)

-- Price in cents
bookPrice :: Int
bookPrice = 800

-- Discount table: group size -> discount factor
discount :: Int -> Double
discount 1 = 1.0
discount 2 = 0.95
discount 3 = 0.90
discount 4 = 0.80
discount 5 = 0.75
discount _ = 1.0

-- Count the number of each book in the basket
bookCounts :: [Book] -> [Int]
bookCounts books = map (\b -> count b books) allBooks
  where
    allBooks = [First, Second, Third, Fourth, Fifth]
    count x = length . filter (== x)

-- Remove a set of size n from the basket
removeSetOfSize :: Int -> [Int] -> [Int]
removeSetOfSize n counts =
  let indices = take n [i | (i, c) <- zip [0..] counts, c > 0]
  in [if i `elem` indices then c - 1 else c | (i, c) <- zip [0..] counts]

-- Find all possible set sizes (number of different books you can take at once)
possibleSetSizes :: [Int] -> [Int]
possibleSetSizes counts = [s | s <- [5,4,3,2,1], canTakeSet s counts]
  where
    canTakeSet s cs = length (filter (>0) cs) >= s

-- Calculate the minimal total price recursively
-- Special handling for the "two groups of four" case
minPrice :: [Int] -> Int
minPrice counts
  | sum counts == 0 = 0
  | otherwise =
      let
        -- All possible set sizes for this state
        setSizes = possibleSetSizes counts
        -- All possible next states and their prices
        prices = [ priceForSet s | s <- setSizes ]
        -- Special case: if we can form more groups of 5 and 3, check if splitting into 4+4 is cheaper
        -- This is the only case where the greedy approach fails
        specialCase = specialFourFour counts
      in maybe (minimum prices) (min (minimum prices)) specialCase
  where
    priceForSet s =
      let setPrice = round $ fromIntegral (s * bookPrice) * discount s
          rest = minPrice (removeSetOfSize s counts)
      in setPrice + rest

-- Special handling: if there are more groups of 5 and 3, try splitting into 4+4
-- Returns Just price if this split is possible and cheaper, otherwise Nothing
specialFourFour :: [Int] -> Maybe Int
specialFourFour counts =
  let
    -- How many groups of 5 can we make?
    n5 = minCount counts
    -- Remove n5 from each nonzero count to get leftovers
    after5 = foldl (\cs _ -> removeSetOfSize 5 cs) counts [1..n5]
    -- How many groups of 3 can we make from leftovers?
    n3 = minCount after5
    -- If both n5 and n3 are at least 1, try splitting into 4+4 instead
    -- (This is only relevant if n5 > 0 and n3 > 0)
  in if n5 > 0 && n3 > 0
     then
       let
         -- Remove one group of 5 and one group of 3
         afterOne5 = removeSetOfSize 5 counts
         afterOne3 = removeSetOfSize 3 afterOne5
         -- Instead, remove two groups of 4
         afterOne4 = removeSetOfSize 4 counts
         afterTwo4 = removeSetOfSize 4 afterOne4
         -- Calculate prices
         price53 = round (5 * fromIntegral bookPrice * discount 5)
                  + round (3 * fromIntegral bookPrice * discount 3)
                  + minPrice afterOne3
         price44 = 2 * round (4 * fromIntegral bookPrice * discount 4)
                  + minPrice afterTwo4
       in Just (min price53 price44)
     else Nothing

-- Helper: minimum count of nonzero elements (i.e., max number of full sets)
minCount :: [Int] -> Int
minCount = minimum . filter (>0)

total :: [Book] -> Int
total books = minPrice (bookCounts books)
