module Dominoes (chain) where

import Data.List (delete)

chain :: [(Int, Int)] -> Maybe [(Int, Int)]
chain [] = Just []
chain ds = tryChain ds ds

tryChain :: [(Int, Int)] -> [(Int, Int)] -> Maybe [(Int, Int)]
tryChain _ [] = Nothing
tryChain avail ((l, r):rest) = 
  case extendChain avail [(l, r)] of
    Just chain' -> Just chain'
    Nothing -> tryChain avail rest

extendChain :: [(Int, Int)] -> [(Int, Int)] -> Maybe [(Int, Int)]
extendChain avail chain@(lastDomino:_) = 
  let lastNum = snd lastDomino
      options = filter (\domino -> fst domino == lastNum || snd domino == lastNum) avail
  in case options of
    [] -> if isValidChain chain then Just chain else Nothing
    _ -> tryOptions (delete lastDomino avail) chain options

tryOptions :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)] -> Maybe [(Int, Int)]
tryOptions _ chain [] = Nothing
tryOptions avail chain@(lastDomino:_) (opt:opts) = 
  let newChain = if fst opt == snd lastDomino then opt:chain else (swap opt):chain
  in case extendChain avail newChain of
    Just chain' -> Just chain'
    Nothing -> tryOptions avail chain opts

isValidChain :: [(Int, Int)] -> Bool
isValidChain chain = 
  let firstNum = fst (head chain)
      lastNum = snd (last chain)
  in firstNum == lastNum

swap :: (Int, Int) -> (Int, Int)
swap (a, b) = (b, a)
