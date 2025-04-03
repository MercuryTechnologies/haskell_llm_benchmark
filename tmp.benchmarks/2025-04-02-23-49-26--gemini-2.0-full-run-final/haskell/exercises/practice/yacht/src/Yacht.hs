module Yacht (yacht, Category(..)) where

data Category = Ones
              | Twos
              | Threes
              | Fours
              | Fives
              | Sixes
              | FullHouse
              | FourOfAKind
              | LittleStraight
              | BigStraight
              | Choice
              | Yacht

yacht :: Category -> [Int] -> Int
yacht category dice =
  case category of
    Ones -> countMultiples 1 dice
    Twos -> countMultiples 2 dice
    Threes -> countMultiples 3 dice
    Fours -> countMultiples 4 dice
    Fives -> countMultiples 5 dice
    Sixes -> countMultiples 6 dice
    FullHouse -> scoreFullHouse dice
    FourOfAKind -> scoreFourOfAKind dice
    LittleStraight -> scoreLittleStraight dice
    BigStraight -> scoreBigStraight dice
    Choice -> sum dice
    Yacht -> scoreYacht dice

countMultiples :: Int -> [Int] -> Int
countMultiples n dice = n * length (filter (== n) dice)

scoreFullHouse :: [Int] -> Int
scoreFullHouse dice
  | isFullHouse dice = sum dice
  | otherwise = 0

isFullHouse :: [Int] -> Bool
isFullHouse dice =
  let counts = map (\x -> length (filter (==x) dice)) dice
      uniqueCounts = nub counts
  in (length uniqueCounts == 2) && (3 `elem` uniqueCounts) && (2 `elem` uniqueCounts)

scoreFourOfAKind :: [Int] -> Int
scoreFourOfAKind dice
  | isFourOfAKind dice = sum $ take 4 $ sortedDice
  | otherwise = 0
  where
    sortedDice = reverse $ sort dice

isFourOfAKind :: [Int] -> Bool
isFourOfAKind dice =
  let counts = map (\x -> length (filter (==x) dice)) dice
  in any (>= 4) counts

scoreLittleStraight :: [Int] -> Int
scoreLittleStraight dice
  | isLittleStraight dice = 30
  | otherwise = 0

isLittleStraight :: [Int] -> Bool
isLittleStraight dice = sort dice == [1,2,3,4,5]

scoreBigStraight :: [Int] -> Int
scoreBigStraight dice
  | isBigStraight dice = 30
  | otherwise = 0

isBigStraight :: [Int] -> Bool
isBigStraight dice = sort dice == [2,3,4,5,6]

scoreYacht :: [Int] -> Int
scoreYacht dice
  | isYacht dice = 50
  | otherwise = 0

isYacht :: [Int] -> Bool
isYacht dice = length (nub dice) == 1

nub :: (Eq a) => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub (filter (/= x) xs)

sort :: (Ord a) => [a] -> [a]
sort = foldr insert []
  where insert x [] = [x]
        insert x (y:ys) | x <= y = x:y:ys
                        | otherwise = y : insert x ys
