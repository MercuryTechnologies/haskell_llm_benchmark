module Dominoes (chain) where

import Data.List (delete)
import Data.Maybe (listToMaybe)

-- | Compute a chain of dominoes where the first and last numbers match.
chain :: [(Int, Int)] -> Maybe [(Int, Int)]
chain [] = Just []  -- Empty list is a valid chain
chain dominoes = listToMaybe $ findChains dominoes

-- | Find all possible valid chains starting with any domino
findChains :: [(Int, Int)] -> [[(Int, Int)]]
findChains [] = [[]]  -- Base case: empty list gives empty chain
findChains dominoes = do
  domino <- dominoes                      -- Try each domino as starting point
  let remaining = delete domino dominoes  -- Remove it from available dominoes
  
  -- Try both orientations of the first domino
  let (a, b) = domino
  chains1 <- extendChain a b remaining     -- Try with original orientation
  return chains1
  ++ do
    chains2 <- extendChain b a remaining   -- Try with flipped orientation
    return chains2

-- | Extend a chain that currently ends with 'end', starting with 'start'
extendChain :: Int -> Int -> [(Int, Int)] -> [[(Int, Int)]]
extendChain start end [] 
  | start == end = [[(start, end)]]  -- Valid chain if first and last match
  | otherwise    = []                -- Invalid chain
extendChain start end dominoes = do
  domino@(a, b) <- dominoes          -- Try each remaining domino
  let remaining = delete domino dominoes
  
  -- If 'a' matches our current end, add domino and continue with 'b' as new end
  chains1 <- if end == a 
             then map ((start, end) :) (extendChain start b remaining)
             else []
             
  -- If 'b' matches our current end, add flipped domino and continue with 'a' as new end
  chains2 <- if end == b 
             then map ((start, end) :) (extendChain start a remaining)
             else []
             
  return $ chains1 ++ chains2
