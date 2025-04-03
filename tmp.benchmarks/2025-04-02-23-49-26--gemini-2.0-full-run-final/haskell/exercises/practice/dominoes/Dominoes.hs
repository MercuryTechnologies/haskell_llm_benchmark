module Dominoes (chain) where

import Control.Monad (msum)

chain :: [(Int, Int)] -> Maybe [(Int, Int)]
chain dominoes = findChain dominoes []

findChain :: [(Int, Int)] -> [(Int, Int)] -> Maybe [(Int, Int)]
findChain [] chainSoFar
  | isChainValid chainSoFar = Just chainSoFar
  | otherwise = Nothing
findChain remainingDominoes chainSoFar = msum $ map tryDomino remainingDominoes
  where
    tryDomino domino = do
      let newRemainingDominoes = removeDomino domino remainingDominoes
      findChain' newRemainingDominoes (chainSoFar ++ [domino])

    findChain' remaining chainSoFar' =
      if null remaining
        then
          if isChainValid chainSoFar'
            then Just chainSoFar'
            else Nothing
        else
          msum $ map tryNextDomino remaining
      where
        tryNextDomino domino =
          if canAppend domino chainSoFar'
            then
              let newRemaining = removeDomino domino remaining
              in findChain' newRemaining (chainSoFar' ++ [domino])
            else
              if canAppend (swapDomino domino) chainSoFar'
                then
                  let newRemaining = removeDomino domino remaining
                  in findChain' newRemaining (chainSoFar' ++ [swapDomino domino])
                else Nothing

removeDomino :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
removeDomino domino dominoes = filter (/= domino) dominoes

canAppend :: (Int, Int) -> [(Int, Int)] -> Bool
canAppend (a, _) [] = True
canAppend (a, _) chain =
  let (_, lastValue) = last chain
  in a == lastValue

swapDomino :: (Int, Int) -> (Int, Int)
swapDomino (a, b) = (b, a)

isChainValid :: [(Int, Int)] -> Bool
isChainValid [] = True
isChainValid chain =
  let (firstValue, _) = head chain
      (_, lastValue) = last chain
  in firstValue == lastValue
