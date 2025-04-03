module Dominoes (chain) where

import Data.List (nubBy)
import Control.Monad (guard)

chain :: [(Int, Int)] -> Maybe [(Int, Int)]
chain [] = Just []
chain dominoes =
  case findChains dominoes of
    (x:_) -> Just x
    []    -> Nothing

findChains :: [(Int, Int)] -> [[(Int, Int)]]
findChains ds = do
  -- Try each unique domino in any orientation as the starting piece
  start <- uniqueDominoes ds
  let (x,y) = start
  newDs <- removeDomino start ds
  dfs x y newDs [(x,y)]

dfs :: Int -> Int -> [(Int, Int)] -> [(Int, Int)] -> [[(Int, Int)]]
dfs start current ds chainSoFar =
  -- If no dominoes left, success if we formed a loop
  if null ds
    then if current == start then return chainSoFar else []
    else do
      -- Try to match any domino that fits the current side
      next <- uniqueDominoes ds
      let (a,b) = next
      guard (a == current || b == current)
      newDs <- removeDomino next ds
      if a == current
        then dfs start b newDs (chainSoFar ++ [(a,b)])
        else dfs start a newDs (chainSoFar ++ [(b,a)])

-- removeDomino attempts to remove exactly one instance of the given domino
-- (either orientation) from the list. Returns [] if it's not present.
removeDomino :: (Int, Int) -> [(Int, Int)] -> [[(Int, Int)]]
removeDomino d ds =
  case safeRemove d ds of
    Just ds' -> [ds']
    Nothing  -> case safeRemove (swap d) ds of
                  Just ds' -> [ds']
                  Nothing  -> []

swap :: (a, b) -> (b, a)
swap (x,y) = (y,x)

-- safeRemove removes the first matching domino from the list if any.
safeRemove :: (Int, Int) -> [(Int, Int)] -> Maybe [(Int, Int)]
safeRemove _ [] = Nothing
safeRemove d (x:xs)
  | sameDomino d x = Just xs
  | otherwise      = (x :) <$> safeRemove d xs

-- uniqueDominoes filters out duplicates disregarding orientation.
uniqueDominoes :: [(Int, Int)] -> [(Int, Int)]
uniqueDominoes []     = []
uniqueDominoes (d:ds) = d : uniqueDominoes (filter (not . sameDomino d) ds)

-- sameDomino matches (x,y) to (y,x) as well
sameDomino :: (Int, Int) -> (Int, Int) -> Bool
sameDomino (x,y) (a,b) = (x == a && y == b) || (x == b && y == a)
