module TwoBucket (measure) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (foldl')

-- | measure (cap1, cap2) target
--   Returns the total number of actions
--   and the final, measured volumes of (bucket1, bucket2).
--   If it's not possible to measure exactly 'target', returns 'Nothing'.
measure :: (Int, Int) -> Int -> Maybe (Int, (Int, Int))
measure (capA, capB) target
  | target < 0             = Nothing
  | target > max capA capB = Nothing
  | otherwise              = bfs (capA, capB) target

-- We'll carry along the number of steps for BFS.
data State = S { stA :: Int
               , stB :: Int
               , steps :: Int
               } deriving (Eq, Ord, Show)

bfs :: (Int, Int) -> Int -> Maybe (Int, (Int, Int))
bfs (capA, capB) target =
  search [S capA 0 1] (Set.fromList [(capA, 0)])
  where
    search :: [State] -> Set (Int, Int) -> Maybe (Int, (Int, Int))
    search [] _ = Nothing
    search (S a b s : queue) visited
      | a == target = Just (s, (a, b))
      | b == target = Just (s, (a, b))
      | otherwise =
          let nexts = nextStates (a, b) capA capB
              validNexts = 
                [ S a' b' (s + 1)
                  | (a', b') <- nexts
                  , not ((a', b') `Set.member` visited)
                  , not (a' == 0 && b' == capB) -- rule violation
                ]
              visited' = foldl' (flip Set.insert) visited [(x, y) | S x y _ <- validNexts]
          in search (queue ++ validNexts) visited'

-- All possible next states from (a, b)
nextStates :: (Int, Int) -> Int -> Int -> [(Int, Int)]
nextStates (a, b) capA capB =
  [ (capA, b)                    -- fill A
  , (a, capB)                    -- fill B
  , (0, b)                       -- empty A
  , (a, 0)                       -- empty B
  , let amount = min a (capB - b)
    in (a - amount, b + amount)  -- pour A -> B
  , let amount = min b (capA - a)
    in (a + amount, b - amount)  -- pour B -> A
  ]
