module Dominoes (chain) where

import Data.List (delete)
import Data.Maybe (listToMaybe)

-- | Given a list of dominoes, return a chain of dominoes where adjacent
-- dominoes match and the first and last dominoes also match.
chain :: [(Int, Int)] -> Maybe [(Int, Int)]
chain [] = Just []  -- Empty list is a valid chain
chain dominoes = listToMaybe $ concatMap tryStartingWith dominoes
  where
    -- Try starting with each domino (in both orientations)
    tryStartingWith d = 
        buildChain [d] (fst d) (delete d dominoes) ++
        buildChain [(snd d, fst d)] (snd d) (delete d dominoes)

    -- Try to build a chain with the given starting value
    buildChain :: [(Int, Int)] -> Int -> [(Int, Int)] -> [[(Int, Int)]]
    buildChain chain targetStart [] = 
        if snd (last chain) == targetStart
            then [chain]
            else []
    buildChain chain targetStart remaining =
        let lastValue = snd (last chain)
        in concatMap (\d -> 
            let newRemaining = delete d remaining
            in
                (if fst d == lastValue
                    then buildChain (chain ++ [d]) targetStart newRemaining
                    else []) ++
                (if snd d == lastValue
                    then buildChain (chain ++ [(snd d, fst d)]) targetStart newRemaining
                    else [])
        ) remaining
