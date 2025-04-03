module TwoBucket (measure) where

import Data.List (foldl')
import qualified Data.Set as Set

-- | measure (capacityOfBucketA, capacityOfBucketB) target
--   returns the minimum number of actions to arrive at a state
--   where one of the buckets has the 'target' liters, along with that final state:
--   Just (actions, (bA, bB))
--   or Nothing if not possible.
measure :: (Int, Int) -> Int -> Maybe (Int, (Int, Int))
measure (capA, capB) target
  | target < 0 || target > max capA capB = Nothing
  | otherwise = bfs (Set.singleton (capA, 0)) [(capA, 0, 1)]
  where
    bfs _ [] = Nothing
    bfs visited ((bA, bB, steps):queue)
      | bA == target = Just (steps, (bA, bB))
      | bB == target = Just (steps, (bA, bB))
      | otherwise =
          let possibleNext = nextStates (capA, capB) (bA, bB)
              validNext =
                filter
                  (\(x, y) -> not (Set.member (x, y) visited) &&
                              not (x == 0 && y == capB))
                  possibleNext
              visited' = foldl' (flip Set.insert) visited validNext
              queue'   = queue ++ map (\(x, y) -> (x, y, steps + 1)) validNext
          in bfs visited' queue'

    nextStates (cA, cB) (bA, bB) =
      [ (cA, bB)                          -- Fill A
      , (bA, cB)                          -- Fill B
      , (0, bB)                           -- Empty A
      , (bA, 0)                           -- Empty B
      , pourAtoB (cA, cB) (bA, bB)        -- Pour A -> B
      , pourBtoA (cA, cB) (bA, bB)        -- Pour B -> A
      ]

    pourAtoB (cA, cB) (bA, bB) =
      let space = cB - bB
      in if bA <= space
         then (0, bB + bA)
         else (bA - space, cB)

    pourBtoA (cA, cB) (bA, bB) =
      let space = cA - bA
      in if bB <= space
         then (bA + bB, 0)
         else (cA, bB - space)
