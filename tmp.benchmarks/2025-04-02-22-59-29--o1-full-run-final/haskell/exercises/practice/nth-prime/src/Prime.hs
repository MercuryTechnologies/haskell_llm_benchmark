module Prime (nth) where

nth :: Int -> Maybe Integer
nth n
    | n <= 0    = Nothing
    | otherwise = Just (go 2 0)
  where
    go x count
      | isPrime x =
          if count + 1 == n
            then x
            else go (x + 1) (count + 1)
      | otherwise = go (x + 1) count

    isPrime x
      | x < 2     = False
      | otherwise = not (any (\d -> x `mod` d == 0)
                             [2 .. floor (sqrt (fromIntegral x))])
