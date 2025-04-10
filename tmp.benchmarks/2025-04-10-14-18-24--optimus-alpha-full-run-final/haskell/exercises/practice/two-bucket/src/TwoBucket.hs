module TwoBucket (measure) where

import Data.List (nub)
import qualified Data.Set as Set

-- | measure (bucket1Capacity, bucket2Capacity) target
-- Returns: Just (actions, (goalBucket, otherBucketLiters))
--   where goalBucket = 1 or 2 (which bucket ends up with the target)
--         otherBucketLiters = how much is in the other bucket
--   or Nothing if impossible
--
-- Always assumes you start by filling bucket 1 first.
measure :: (Int, Int) -> Int -> Maybe (Int, (Int, Int))
measure (cap1, cap2) target
  | target > cap1 && target > cap2 = Nothing
  | target == cap1 = Just (1, (1, 0))
  | target == cap2 = Just (2, (0, cap2))
  | otherwise =
      case bfs (cap1, cap2) target of
        Just (actions, (b1, b2)) ->
          if b1 == target then Just (actions, (1, b2))
          else if b2 == target then Just (actions, (2, b1))
          else Nothing
        Nothing -> Nothing

type State = (Int, Int) -- (bucket1, bucket2)

-- BFS to find the shortest sequence of actions
bfs :: (Int, Int) -> Int -> Maybe (Int, State)
bfs (cap1, cap2) target = go Set.empty [((1,0), 1)] -- start: fill bucket 1
  where
    go :: Set.Set State -> [(State, Int)] -> Maybe (Int, State)
    go _ [] = Nothing
    go visited ((s@(b1, b2), n):rest)
      | b1 == target = Just (n, s)
      | b2 == target = Just (n, s)
      | Set.member s visited = go visited rest
      | otherwise =
          let visited' = Set.insert s visited
              nexts = filter (not . (`Set.member` visited') . fst) $ map (\st -> (st, n+1)) (nextStates s)
          in go visited' (rest ++ nexts)

    -- Generate all valid next states from current state
    nextStates :: State -> [State]
    nextStates (b1, b2) = nub $ filter validState
      [ -- Fill bucket 1
        (cap1, b2)
      , -- Fill bucket 2
        (b1, cap2)
      , -- Empty bucket 1
        (0, b2)
      , -- Empty bucket 2
        (b1, 0)
      , -- Pour bucket 1 -> bucket 2
        let pour = min b1 (cap2 - b2) in (b1 - pour, b2 + pour)
      , -- Pour bucket 2 -> bucket 1
        let pour = min b2 (cap1 - b1) in (b1 + pour, b2 - pour)
      ]

    -- Rule: may not arrive at a state where starting bucket is empty and the other is full
    validState :: State -> Bool
    validState (b1, b2) = not (b1 == 0 && b2 == cap2)

