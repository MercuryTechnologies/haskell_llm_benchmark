module Dominoes (chain) where

import Data.List (delete)

-- Try to build a chain recursively
chain :: [(Int, Int)] -> Maybe [(Int, Int)]
chain [] = Just []
chain [d@(a, b)]
  | a == b    = Just [d]
  | otherwise = Nothing
chain dominoes = tryChain dominoes []

-- Try to build a chain, accumulating the current chain in acc
tryChain :: [(Int, Int)] -> [(Int, Int)] -> Maybe [(Int, Int)]
tryChain [] acc
  | null acc = Just []
  | fst (head acc) == snd (last acc) = Just (reverse acc)
  | otherwise = Nothing
tryChain dominoes [] =
  -- Try every domino as the starting domino, in both orientations
  tryFirst dominoes
  where
    tryFirst [] = Nothing
    tryFirst (d:ds) =
      case tryChain (delete d dominoes) [d] of
        Just res -> Just res
        Nothing ->
          let d' = swap d
          in case tryChain (delete d dominoes) [d'] of
               Just res -> Just res
               Nothing -> tryFirst ds
tryChain dominoes acc =
  let end = snd (head acc)
      tryNext [] = Nothing
      tryNext (d:ds) =
        if fst d == end
          then case tryChain (delete d dominoes) (d:acc) of
                 Just res -> Just res
                 Nothing -> tryNext ds
        else if snd d == end
          then case tryChain (delete d dominoes) (swap d:acc) of
                 Just res -> Just res
                 Nothing -> tryNext ds
        else tryNext ds
  in tryNext dominoes

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)
