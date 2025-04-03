module Dominoes (chain) where

import Data.List (delete) -- Used for removing elements from the list of remaining dominoes
import Data.Maybe (listToMaybe) -- Used to return the first valid chain found, or Nothing

-- Helper function to swap the halves of a domino
swapDomino :: (Int, Int) -> (Int, Int)
swapDomino (a, b) = (b, a)

-- Main function to find a domino chain
chain :: [(Int, Int)] -> Maybe [(Int, Int)]
chain [] = Just [] -- An empty list of dominoes forms a valid (empty) chain
chain dominoes = listToMaybe $ findChains dominoes -- Find the first valid chain, if any

-- Tries to find all possible valid chains by starting with each domino
findChains :: [(Int, Int)] -> [[(Int, Int)]]
findChains dominoes =
  [ finalChain
  | -- Iterate through each domino index to select a starting domino
    idx <- [0 .. length dominoes - 1],
    -- Pick the starting domino and get the list of remaining dominoes
    let (startDomino, remaining) = pick dominoes idx,
    -- Try both orientations for the starting domino
    orientedStart <- [startDomino, swapDomino startDomino],
    -- Recursively attempt to build a chain starting with this domino and orientation
    finalChain <- buildChain remaining [orientedStart]
  ]

-- Helper function to pick the element at index `idx` and return it
-- along with the rest of the list (excluding the picked element).
-- Note: This implementation is simple but not the most efficient for large lists.
pick :: [a] -> Int -> (a, [a])
pick xs idx =
  let item = xs !! idx
      (before, after) = splitAt idx xs
   in (item, before ++ drop 1 after) -- Combine parts before and after the picked element

-- Recursive function to build the chain step by step
buildChain :: [(Int, Int)] -> [(Int, Int)] -> [[(Int, Int)]]
buildChain [] currentChain =
  -- Base case: No dominoes left to add. Check if the chain is circular.
  let (firstNum, _) = head currentChain -- Get the first number of the first domino
      (_, lastNum) = last currentChain -- Get the last number of the last domino
   in if firstNum == lastNum -- Check if the first and last numbers match
        then [currentChain] -- If they match, this is a valid chain
        else [] -- Otherwise, it's not a valid circular chain
buildChain remaining currentChain =
  -- Recursive step: Try to add another domino to the chain.
  let (_, lastNum) = last currentChain -- Get the number we need to match on the next domino
   in [ finalChain
      | -- Iterate through the indices of the remaining dominoes
        idx <- [0 .. length remaining - 1],
        -- Pick the next potential domino and the rest of the list
        let (nextDomino@(p, q), rest) = pick remaining idx,
        -- Determine the correct orientation for the next domino, if it matches
        orientedNext <- case nextDomino of
                          (p', q') | p' == lastNum -> [(p', q')] -- Matches as is (p' connects to lastNum)
                                   | q' == lastNum -> [(q', p')] -- Matches when swapped (q' connects to lastNum)
                                   | otherwise     -> []         -- Does not connect
        ,
        -- If a connecting orientation is found, recursively continue building the chain
        -- Wrap orientedNext in a list to match the type expected by (++)
        finalChain <- buildChain rest (currentChain ++ [orientedNext])
      ]
