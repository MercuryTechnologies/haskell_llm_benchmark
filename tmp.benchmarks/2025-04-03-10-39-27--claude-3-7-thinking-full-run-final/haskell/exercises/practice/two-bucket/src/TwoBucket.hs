module TwoBucket (measure) where

data Bucket = One | Two deriving (Eq, Show)

-- | Given the capacities of two buckets and a target volume, determine how many actions
-- are required to measure the target volume, which bucket contains the target volume,
-- and how much is in the other bucket.
-- We always start by filling bucket 1.
-- Returns (num_actions, (bucket_with_target, volume_in_other_bucket))
measure :: (Int, Int) -> Int -> Maybe (Int, (Int, Int))
measure (capacity1, capacity2) target
  | target > max capacity1 capacity2 = Nothing  -- Target is larger than both buckets
  | target == 0 = Just (2, (1, 0))  -- Target is 0, fill bucket 1 then empty it
  | target == capacity1 = Just (1, (1, 0))  -- Target is capacity1, just fill bucket 1
  | otherwise = bfs [((capacity1, 0), 1)] []
  where
    -- BFS implementation using lists instead of Maps
    bfs [] _ = Nothing  -- No more states to explore, no solution
    bfs (((b1, b2), steps):rest) visited
      | b1 == target = Just (steps, (1, b2))  -- Found target in bucket 1
      | b2 == target = Just (steps, (2, b1))  -- Found target in bucket 2
      | (b1, b2) `elem` visited = bfs rest visited  -- Skip if already visited
      | otherwise = bfs (rest ++ nextStatesWithSteps) ((b1, b2) : visited)  -- Continue BFS
      where
        nextStates = filter validState (getNextStates (b1, b2))
        nextStatesWithSteps = [(s, steps + 1) | s <- nextStates, s `notElem` visited]
    
    -- Check if a state is valid (not violating the rule)
    validState (0, b2) = b2 /= capacity2  -- Starting bucket empty and other bucket full is invalid
    validState _ = True
    
    -- Get all possible next states from the current state
    getNextStates (b1, b2) = [
      (capacity1, b2),   -- Fill bucket 1
      (b1, capacity2),   -- Fill bucket 2
      (0, b2),           -- Empty bucket 1
      (b1, 0),           -- Empty bucket 2
      (b1', b2'),        -- Pour from bucket 1 to bucket 2
      (b1'', b2'')       -- Pour from bucket 2 to bucket 1
      ]
      where
        -- Pour from bucket 1 to bucket 2
        pourAmount1 = min b1 (capacity2 - b2)
        b1' = b1 - pourAmount1
        b2' = b2 + pourAmount1
        
        -- Pour from bucket 2 to bucket 1
        pourAmount2 = min b2 (capacity1 - b1)
        b1'' = b1 + pourAmount2
        b2'' = b2 - pourAmount2
