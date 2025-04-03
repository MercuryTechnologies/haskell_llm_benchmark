module BookStore (total, Book(..)) where

import Data.List (sort, sortBy)

data Book = First | Second | Third | Fourth | Fifth
  deriving (Eq, Ord)

-- The idea: 
-- Price is 800 cents per book, with discounts if we form groups of distinct books:
--   2 distinct => 5% discount     => 760 cents/book
--   3 distinct => 10% discount    => 720 cents/book
--   4 distinct => 20% discount    => 640 cents/book
--   5 distinct => 25% discount    => 600 cents/book
--
-- We want the arrangement of sets of distinct books that gives the minimal total cost.
-- For the famous "5+3 vs 4+4" scenario, converting one (5,3) pair into two sets of 4 is cheaper.

-- Cost per size of distinct group:
costPerBook :: Int -> Int
costPerBook 1 = 800
costPerBook 2 = 760
costPerBook 3 = 720
costPerBook 4 = 640
costPerBook 5 = 600
costPerBook _ = 800  -- fallback

total :: [Book] -> Int
total basket =
  let
    -- Count how many copies of each Book.
    freq = map length . groupByEq . sort $ basket

    -- We'll repeatedly take the largest possible set (up to 5) each time
    -- until no frequencies remain > 0.
    (s1, s2, s3, s4, s5) = pickSets freq 0 0 0 0 0

    -- Adjust for "5+3 vs 4+4" optimization. It's cheaper to replace
    -- 1 group of 5 plus 1 group of 3 with 2 groups of 4 if possible.
    (finalS3, finalS4, finalS5) = convert53To44 s3 s4 s5

  in
    s1 * costPerGroup 1 +
    s2 * costPerGroup 2 +
    finalS3 * costPerGroup 3 +
    finalS4 * costPerGroup 4 +
    finalS5 * costPerGroup 5

pickSets :: [Int] -> Int -> Int -> Int -> Int -> Int -> (Int, Int, Int, Int, Int)
pickSets freqs s1 s2 s3 s4 s5 =
  let sortedDesc = sortBy (flip compare) freqs
      distinctCount = length (filter (> 0) sortedDesc)
  in
    case distinctCount of
      0 -> (s1, s2, s3, s4, s5)
      _ ->
         -- We want to form the largest set possible (up to 5):
         let setSize = min 5 distinctCount
             updated = subtractOneFromTopN setSize sortedDesc
         in
           case setSize of
             5 -> pickSets updated s1 s2 s3 s4 (s5 + 1)
             4 -> pickSets updated s1 s2 s3 (s4 + 1) s5
             3 -> pickSets updated s1 s2 (s3 + 1) s4 s5
             2 -> pickSets updated s1 (s2 + 1) s3 s4 s5
             1 -> pickSets updated (s1 + 1) s2 s3 s4 s5

subtractOneFromTopN :: Int -> [Int] -> [Int]
subtractOneFromTopN _ [] = []
subtractOneFromTopN 0 xs = xs
subtractOneFromTopN n (x:xs)
  | x > 0     = (x - 1) : subtractOneFromTopN (n - 1) xs
  | otherwise = x       : subtractOneFromTopN n xs

convert53To44 :: Int -> Int -> Int -> (Int, Int, Int)
convert53To44 s3 s4 s5
  | s3 > 0 && s5 > 0 && cost53 > cost44 =
      convert53To44 (s3 - 1) (s4 + 2) (s5 - 1)
  | otherwise =
      (s3, s4, s5)
  where
    cost53 = costPerGroup 5 + costPerGroup 3
    cost44 = 2 * costPerGroup 4

costPerGroup :: Int -> Int
costPerGroup n = n * costPerBook n

groupByEq :: Eq a => [a] -> [[a]]
groupByEq [] = []
groupByEq (x:xs) =
  let (prefix, rest) = span (== x) xs
  in (x : prefix) : groupByEq rest
