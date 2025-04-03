module BookStore (total, Book(..)) where

data Book = First | Second | Third | Fourth | Fifth deriving (Eq, Show)

pricePerBook :: Int
pricePerBook = 800  -- price in cents

discounts :: [(Int, Float)]
discounts = [(2, 0.05), (3, 0.10), (4, 0.20), (5, 0.25)]

total :: [Book] -> Int
total basket = sum $ map calculateGroupPrice groupedBooks
  where
    groupedBooks = groupBooks basket

groupBooks :: [Book] -> [[Book]]
groupBooks [] = []
groupBooks books = let uniqueBooks = length $ distinct books
                       groupSize = min uniqueBooks 5
                   in take groupSize (distinct books) : groupBooks (removeBooks (take groupSize (distinct books)) books)

distinct :: Eq a => [a] -> [a]
distinct [] = []
distinct (x:xs) = x : distinct (filter (/= x) xs)

removeBooks :: Eq a => [a] -> [a] -> [a]
removeBooks _ [] = []
removeBooks toRemove books = filter (`notElem` toRemove) books

calculateGroupPrice :: [Book] -> Int
calculateGroupPrice group = let groupSize = length group
                                discount = lookup groupSize discounts
                                discountFactor = case discount of
                                    Just d  -> 1 - d
                                    Nothing -> 1
                            in floor (fromIntegral groupSize * fromIntegral pricePerBook * discountFactor)
