module TwoBucket (measure) where

measure :: (Int, Int) -> Int -> Maybe (Int, (Int, Int))
measure (capacity1, capacity2) target
  | target > max capacity1 capacity2 = Nothing
  | otherwise = solve capacity1 capacity2 target

solve :: Int -> Int -> Int -> Maybe (Int, (Int, Int))
solve capacity1 capacity2 target =
  let
    -- Helper function to perform the steps
    helper :: Int -> Int -> Int -> Int -> Int -> Int -> Maybe (Int, (Int, Int))
    helper bucket1 bucket2 target steps cap1 cap2 firstBucketFilled
      | bucket1 == target = Just (steps, (bucket1, bucket2))
      | bucket2 == target = Just (steps, (bucket1, bucket2))
      | steps > 10 = Nothing  -- Limit steps to avoid infinite loops

      | otherwise =
        let
          -- Possible actions:
          -- 1. Fill bucket1
          fill1 = if bucket1 == cap1 then Nothing else helper cap1 bucket2 target (steps + 1) cap1 cap2 firstBucketFilled
          -- 2. Fill bucket2
          fill2 = if bucket2 == cap2 then Nothing else helper bucket1 cap2 target (steps + 1) cap1 cap2 firstBucketFilled
          -- 3. Empty bucket1
          empty1 = if bucket1 == 0 then Nothing else helper 0 bucket2 target (steps + 1) cap1 cap2 firstBucketFilled
          -- 4. Empty bucket2
          empty2 = if bucket2 == 0 then Nothing else helper bucket1 0 target (steps + 1) cap1 cap2 firstBucketFilled
          -- 5. Pour bucket1 into bucket2
          pour1to2 = if bucket1 == 0 || bucket2 == cap2 then Nothing else let pourAmount = min bucket1 (cap2 - bucket2) in helper (bucket1 - pourAmount) (bucket2 + pourAmount) target (steps + 1) cap1 cap2 firstBucketFilled
          -- 6. Pour bucket2 into bucket1
          pour2to1 = if bucket2 == 0 || bucket1 == cap1 then Nothing else let pourAmount = min bucket2 (cap1 - bucket1) in helper (bucket1 + pourAmount) (bucket2 - pourAmount) target (steps + 1) cap1 cap2 firstBucketFilled
        in
          -- Prioritize filling the first bucket if it's the first move, otherwise explore other options
          if steps == 0
            then
              case helper cap1 0 target (steps + 1) cap1 cap2 True of
                Just res -> Just res
                Nothing -> Nothing
            else
              case fill1 of
                Just res -> Just res
                Nothing -> case fill2 of
                  Just res -> Just res
                  Nothing -> case empty1 of
                    Just res -> Just res
                    Nothing -> case empty2 of
                      Just res -> Just res
                      Nothing -> case pour1to2 of
                        Just res -> Just res
                        Nothing -> case pour2to1 of
                          Just res -> Just res
                          Nothing -> Nothing
  in
    helper 0 0 target 0 capacity1 capacity2 False
