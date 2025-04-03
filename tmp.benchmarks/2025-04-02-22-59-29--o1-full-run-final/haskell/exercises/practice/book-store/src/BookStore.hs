module BookStore (total, Book(..)) where

import Data.List (sort)

data Book = First
          | Second
          | Third
          | Fourth
          | Fifth
  deriving (Eq, Show)

-- | The price of grouping n distinct books (in cents).
groupCost :: Int -> Int
groupCost 1 = 800
groupCost 2 = 1520
groupCost 3 = 2160
groupCost 4 = 2560
groupCost 5 = 3000
groupCost _ = error "Invalid group size"

-- | Count how many times each Book constructor appears.
countBooks :: [Book] -> (Int, Int, Int, Int, Int)
countBooks = foldr add (0,0,0,0,0)
  where
    add First  (a,b,c,d,e) = (a+1, b  , c  , d  , e  )
    add Second (a,b,c,d,e) = (a  , b+1, c  , d  , e  )
    add Third  (a,b,c,d,e) = (a  , b  , c+1, d  , e  )
    add Fourth (a,b,c,d,e) = (a  , b  , c  , d+1, e  )
    add Fifth  (a,b,c,d,e) = (a  , b  , c  , d  , e+1)

-- | Repeatedly form groups of different books (up to 5 distinct).
--   For each group, we reduce the count of each distinct Book by one.
--   We collect the group sizes in a list, e.g. [5, 3, 4].
greedyGroupSizes :: (Int,Int,Int,Int,Int) -> [Int]
greedyGroupSizes = go []
  where
    go acc (0,0,0,0,0) = reverse acc
    go acc (a,b,c,d,e) =
      let counts = [a,b,c,d,e]
          distinct = length (filter (>0) counts)
          size = min distinct 5  -- can't exceed 5 anyway
          -- Subtract 1 from the first 'size' non-zero slots
          newCounts = pick counts size
          (a',b',c',d',e') = tuple5 newCounts
      in go (size : acc) (a',b',c',d',e')

    pick [] _ = []
    pick (x:xs) n
      | n == 0    = x : xs
      | x > 0     = (x-1) : pick xs (n-1)
      | otherwise = x     : pick xs n

    tuple5 [p,q,r,s,t] = (p,q,r,s,t)
    tuple5 _           = error "tuple5: unexpected list length"

-- | If we have sets of size 5 and sets of size 3, converting them into two sets of size 4 each can be cheaper.
--   cost(5) + cost(3) = 3000 + 2160 = 5160
--   cost(4) + cost(4) = 2560 + 2560 = 5120
--   So if there's at least one group of size 5 and one of size 3, replace them with two groups of size 4.
correctGroups :: [Int] -> [Int]
correctGroups sizes =
  let freq5 = length (filter (==5) sizes)
      freq3 = length (filter (==3) sizes)
      canImprove = freq5 > 0 && freq3 > 0
      cost53 = groupCost 5 + groupCost 3
      cost44 = groupCost 4 + groupCost 4
  in if canImprove && cost53 > cost44
        then correctGroups (removeOne 5 (removeOne 3 sizes) ++ [4,4])
        else sizes

  where
    removeOne _ [] = []
    removeOne x (y:ys)
      | x == y    = ys
      | otherwise = y : removeOne x ys

-- | Compute the total price in cents for the given basket of books.
total :: [Book] -> Int
total basket =
  let counts      = countBooks basket
      groupSizes  = greedyGroupSizes counts
      improved    = correctGroups groupSizes
  in sum (map groupCost improved)
