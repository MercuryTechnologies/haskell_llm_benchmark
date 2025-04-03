module BookStore (total, Book(..)) where

import Data.List (maximumBy)

data Book = First | Second | Third | Fourth | Fifth deriving (Eq, Ord, Enum, Bounded)

price :: Int -> Int
price 1 = 800
price 2 = 1520
price 3 = 2160
price 4 = 2560
price 5 = 3000
price _ = 0

groupBooks :: [Book] -> [[Book]]
groupBooks [] = []
groupBooks books = bestGrouping books []
  where
    bestGrouping :: [Book] -> [[Book]] -> [[Book]]
    bestGrouping remaining currentGrouping =
      case findBestGroup remaining of
        Nothing -> currentGrouping ++ [remaining]
        Just group -> bestGrouping (remaining \\ group) (currentGrouping ++ [group])

    findBestGroup :: [Book] -> Maybe [Book]
    findBestGroup remaining =
      maximumByDiscount remaining [ [book | book <- [First .. Fifth], book `elem` remaining] | i <- [5,4,3,2,1]]

    maximumByDiscount :: [Book] -> [[Book]] -> Maybe [Book]
    maximumByDiscount remaining groups =
      case groups of
        [] -> Nothing
        _ -> Just $ maximumBy (\g1 g2 -> compare (discount g1) (discount g2)) (filter (\g -> all (`elem` remaining) g && length g > 0) groups)

    discount :: [Book] -> Int
    discount group = price (length group)

    (\\) :: (Eq a) => [a] -> [a] -> [a]
    xs \\ ys = filter (`notElem` ys) xs

total :: [Book] -> Int
total books = sum $ map (price . length) (groupBooks books)
