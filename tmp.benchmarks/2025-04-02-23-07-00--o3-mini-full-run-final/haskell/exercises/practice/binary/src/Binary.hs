module Binary (toDecimal) where

toDecimal :: String -> Int
toDecimal xs
  | any (\c -> c /= '0' && c /= '1') xs = error "invalid input"
  | otherwise = foldl (\acc c -> acc * 2 + digit c) 0 xs
  where
    digit '0' = 0
    digit '1' = 1
    digit _   = error "Logic error in digit"
