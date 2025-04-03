module BookStore (total, Book(..)) where

import qualified Data.Map.Strict as Map
import Control.Monad.State (State, evalState, get, modify)
import Data.List (foldl')

data Book = First | Second | Third | Fourth | Fifth
  deriving (Eq, Ord, Show)

unitPrice :: Int
unitPrice = 800

groupCost :: Int -> Int
groupCost n = case n of
  1 -> 1 * unitPrice
  2 -> (2 * unitPrice * 95) `div` 100
  3 -> (3 * unitPrice * 90) `div` 100
  4 -> (4 * unitPrice * 80) `div` 100
  5 -> (5 * unitPrice * 75) `div` 100
  _ -> error "Invalid group size"

-- | 'total' computes the minimum price (in cents) for a given basket.
total :: [Book] -> Int
total basket = evalState (dp counts) Map.empty
  where
    -- Create a list of counts for each of the 5 books in order [First..Fifth]
    counts :: [Int]
    counts = foldl' (\cs b -> let idx = bookIndex b in updateCount cs idx) (replicate 5 0) basket

    bookIndex :: Book -> Int
    bookIndex b = case b of
      First  -> 0
      Second -> 1
      Third  -> 2
      Fourth -> 3
      Fifth  -> 4

    updateCount :: [Int] -> Int -> [Int]
    updateCount cs idx =
      let (before, x:after) = splitAt idx cs
      in before ++ (x + 1) : after

    -- dp uses memoization (via a State monad holding a Map) to compute the minimum cost from a given state.
    dp :: [Int] -> State (Map.Map [Int] Int) Int
    dp cs
      | all (==0) cs = return 0
      | otherwise = do
          memo <- get
          case Map.lookup cs memo of
            Just result -> return result
            Nothing -> do
              let available = [i | (i, c) <- zip [0..] cs, c > 0]
                  subsets = nonEmptySubsets available
              costs <- mapM (\subset -> do
                               costRest <- dp (removeBooks cs subset)
                               return (groupCost (length subset) + costRest)
                            ) subsets
              let result = minimum costs
              modify (Map.insert cs result)
              return result

    -- Subtract 1 from the count of each book in the given subset of indices.
    removeBooks :: [Int] -> [Int] -> [Int]
    removeBooks cs subset = [ if i `elem` subset then c - 1 else c | (i, c) <- zip [0..] cs ]

    -- Generate all non-empty subsets of a list.
    nonEmptySubsets :: [Int] -> [[Int]]
    nonEmptySubsets xs = filter (not . null) (subsets xs)

    subsets :: [Int] -> [[Int]]
    subsets [] = [[]]
    subsets (x:xs) = let rest = subsets xs in rest ++ map (x:) rest
