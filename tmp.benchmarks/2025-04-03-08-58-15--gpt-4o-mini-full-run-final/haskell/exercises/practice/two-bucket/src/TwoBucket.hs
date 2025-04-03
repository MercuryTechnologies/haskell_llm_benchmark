module TwoBucket (measure) where

measure :: (Int, Int) -> Int -> Maybe (Int, (Int, Int))
measure (capacity1, capacity2) target
    | target > max capacity1 capacity2 = Nothing
    | otherwise = Just (steps, (bucket1, bucket2))
  where
    (steps, (bucket1, bucket2)) = fillBuckets capacity1 capacity2 target

fillBuckets :: Int -> Int -> Int -> (Int, (Int, Int))
fillBuckets capacity1 capacity2 target = go 0 (0, 0)
  where
    go steps (b1, b2)
        | b1 == target = (steps, (b1, b2))
        | b2 == target = (steps, (b1, b2))
        | otherwise = go (steps + 1) (nextState b1 b2)

    nextState b1 b2
        | b1 == 0 = (capacity1, b2)  -- Fill bucket 1
        | b2 == capacity2 = (b1, 0)  -- Empty bucket 2
        | otherwise = pour b1 b2

    pour b1 b2 = (newB1, newB2)
      where
        newB1 = max 0 (b1 - (capacity2 - b2))
        newB2 = min capacity2 (b2 + b1)

    -- Ensure we do not end up with the starting bucket empty and the other full
    validState b1 b2 = not (b1 == 0 && b2 == capacity2)
