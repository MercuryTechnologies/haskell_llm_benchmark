module BookStore (total, Book(..)) where

data Book = First | Second | Third | Fourth | Fifth deriving (Eq)

total :: [Book] -> Int
total basket = minimum [calculatePrice order counts | order <- orders]
  where
    counts = countBooks basket
    
    -- Try different ordering strategies for forming groups
    orders = [
        [5, 4, 3, 2, 1],
        [4, 5, 3, 2, 1],
        [4, 3, 5, 2, 1],
        [3, 4, 5, 2, 1],
        [3, 5, 4, 2, 1],
        [5, 3, 4, 2, 1]
      ]
    
    -- Count occurrences of each book type
    countBooks :: [Book] -> [Int]
    countBooks bs = [count First, count Second, count Third, count Fourth, count Fifth]
      where
        count book = length $ filter (== book) bs
    
    -- Calculate total price using a specific ordering strategy
    calculatePrice :: [Int] -> [Int] -> Int
    calculatePrice [] _ = 0
    calculatePrice _ counts | all (== 0) counts = 0
    calculatePrice (size:sizes) counts
      | canMakeGroup size counts = 
          let (newCounts, bookPrice) = extractGroup size counts
          in bookPrice + calculatePrice (size:sizes) newCounts
      | otherwise = calculatePrice sizes counts
    
    -- Check if we can form a group of a specific size
    canMakeGroup :: Int -> [Int] -> Bool
    canMakeGroup size counts = length (filter (> 0) counts) >= size
    
    -- Extract a group of a specific size from the counts
    extractGroup :: Int -> [Int] -> ([Int], Int)
    extractGroup size counts =
      let indices = take size [i | (i, c) <- zip [0..] counts, c > 0]
          newCounts = zipWith (\i c -> if i `elem` indices then c - 1 else c) [0..] counts
      in (newCounts, groupPrice size)
    
    -- Calculate the price for a group of a specific size
    groupPrice :: Int -> Int
    groupPrice size = size * 800 * (100 - discount size) `div` 100
      where
        discount 1 = 0
        discount 2 = 5
        discount 3 = 10
        discount 4 = 20
        discount 5 = 25
