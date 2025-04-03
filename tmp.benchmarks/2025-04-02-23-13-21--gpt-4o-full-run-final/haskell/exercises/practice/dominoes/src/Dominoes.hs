module Dominoes (chain) where

import Data.List (delete)
import Control.Applicative ((<|>))

chain :: [(Int, Int)] -> Maybe [(Int, Int)]
chain [] = Just []
chain dominoes = findChain dominoes []

findChain :: [(Int, Int)] -> [(Int, Int)] -> Maybe [(Int, Int)]
findChain [] acc
  | isValidChain acc = Just acc
  | otherwise = Nothing
findChain dominoes acc = 
  let candidates = if null acc then dominoes else filter (\(x, _) -> x == snd (last acc) || x == fst (last acc)) dominoes
  in foldr (\domino acc -> acc <|> tryDomino domino dominoes acc) Nothing candidates

tryDomino :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)] -> Maybe [(Int, Int)]
tryDomino domino dominoes acc = 
  let remaining = delete domino dominoes
      newAcc = acc ++ [domino]
      flippedDomino = (snd domino, fst domino)
  in findChain remaining newAcc <|> findChain (delete flippedDomino dominoes) (acc ++ [flippedDomino])

isValidChain :: [(Int, Int)] -> Bool
isValidChain [] = True
isValidChain chain = fst (head chain) == snd (last chain)
