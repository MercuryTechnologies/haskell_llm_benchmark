module Dominoes (chain) where

import Data.List (delete)
import Control.Applicative ((<|>))

chain :: [(Int, Int)] -> Maybe [(Int, Int)]
chain [] = Just []
chain dominoes = tryChain dominoes []

tryChain :: [(Int, Int)] -> [(Int, Int)] -> Maybe [(Int, Int)]
tryChain [] acc
  | null acc = Just []
  | otherwise =
      let (startL, _) = head acc
          (_, endR) = last acc
       in if startL == endR then Just acc else Nothing
tryChain remaining [] =
  -- Try each domino as starting point, in both orientations
  foldr
    (\d acc ->
       acc <|> tryChain (delete d remaining) [d]
           <|> tryChain (delete d remaining) [swap d])
    Nothing
    remaining
tryChain remaining acc =
  let (_, endR) = last acc
   in foldr
        (\d accResult ->
           accResult <|>
             (if fst d == endR
                then tryChain (delete d remaining) (acc ++ [d])
                else if snd d == endR
                       then tryChain (delete d remaining) (acc ++ [swap d])
                       else Nothing))
        Nothing
        remaining

swap :: (a, a) -> (a, a)
swap (x, y) = (y, x)
