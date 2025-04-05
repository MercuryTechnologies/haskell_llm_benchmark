module TwoBucket (measure) where

import Data.List (nub)
import qualified Data.Set as Set

type State = (Int, Int)
type Capacity = (Int, Int)

measure :: (Int, Int) -> Int -> Maybe (Int, (Int, Int))
measure (cap1, cap2) target
  | target > cap1 && target > cap2 = Nothing
  | target `mod` gcd cap1 cap2 /= 0 = Nothing
  | otherwise = case (bfs (cap1, cap2) target (cap1, 0), bfs (cap2, cap1) target (0, cap2)) of
      (Nothing, Nothing) -> Nothing
      (Just res1, Nothing) -> Just res1
      (Nothing, Just res2) -> Just (adjustResult res2)
      (Just res1, Just res2) ->
        if fst3 res1 <= fst3 (adjustResult res2)
          then Just res1
          else Just (adjustResult res2)
  where
    fst3 (a, _, _) = a
    adjustResult (moves, (x, y)) = (moves, (y, x))

bfs :: Capacity -> Int -> State -> Maybe (Int, State)
bfs (cap1, cap2) target start = bfs' Set.empty [(start, 1)]
  where
    bfs' _ [] = Nothing
    bfs' visited ((state@(x, y), moves) : rest)
      | x == target = Just (moves, state)
      | y == target = Just (moves, state)
      | otherwise =
          let nextStates = filter validState $ next state
              newStates = [ (s, moves + 1) | s <- nextStates, Set.notMember s visited ]
              visited' = foldr (Set.insert . fst) visited newStates
          in bfs' visited' (rest ++ newStates)

    validState (x, y) = not (x == 0 && y == cap2)

    next (x, y) = nub
      [ (cap1, y)            -- fill bucket 1
      , (x, cap2)            -- fill bucket 2
      , (0, y)               -- empty bucket 1
      , (x, 0)               -- empty bucket 2
      , pour x y cap2        -- pour bucket 1 -> bucket 2
      , pour y x cap1 >>= \(y', x') -> Just (x', y') -- pour bucket 2 -> bucket 1
      ] >>= maybeToList

    pour from to toCap
      | total <= toCap = Just (0, total)
      | otherwise = Just (from - (toCap - to), toCap)
      where total = from + to

    maybeToList (Just x) = [x]
    maybeToList Nothing = []

