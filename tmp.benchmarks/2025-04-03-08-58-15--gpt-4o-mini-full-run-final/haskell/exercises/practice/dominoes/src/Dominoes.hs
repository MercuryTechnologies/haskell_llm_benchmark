module Dominoes (chain) where

import Data.Maybe (listToMaybe)
import Data.List (nub)
import Control.Monad (msum)

chain :: [(Int, Int)] -> Maybe [(Int, Int)]
chain dominoes = findChain (nub dominoes) []

findChain :: [(Int, Int)] -> [(Int, Int)] -> Maybe [(Int, Int)]
findChain [] currentChain
  | isValidChain currentChain = Just currentChain
  | otherwise = Nothing
findChain remaining currentChain = 
  msum [findChain (removeDomino d remaining) (currentChain ++ [d]) | d <- remaining, canAddToChain d currentChain]

canAddToChain :: (Int, Int) -> [(Int, Int)] -> Bool
canAddToChain (a, b) [] = True
canAddToChain (a, b) chain = lastHalf == a || lastHalf == b
  where lastHalf = snd (last chain)

removeDomino :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
removeDomino d = filter (/= d)

isValidChain :: [(Int, Int)] -> Bool
isValidChain [] = False
isValidChain chain = fst (head chain) == snd (last chain)
