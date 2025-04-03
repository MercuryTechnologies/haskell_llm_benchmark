module TwoBucket (measure) where

measure :: (Int, Int) -> Int -> Maybe (Int, (Int, Int))
measure (cap1, cap2) target
  | target > max cap1 cap2         = Nothing
  | target `mod` gcd cap1 cap2 /= 0  = Nothing
  | otherwise                      = Just (simulate (cap1, 0) 1)
  where
    simulate (x, y) moves
      | x == target = (moves, (2, y))
      | y == target = (moves, (1, x))
      | x == 0      = simulate (cap1, y) (moves + 1)   -- fill bucket one
      | y == cap2   = simulate (x, 0) (moves + 1)        -- empty bucket two
      | x == (cap2 - y) = simulate (x, 0) (moves + 1)      -- pouring would yield the forbidden state so empty bucket two instead
      | otherwise   = let pour = min x (cap2 - y)
                          nextState = (x - pour, y + pour)
                      in simulate nextState (moves + 1)
