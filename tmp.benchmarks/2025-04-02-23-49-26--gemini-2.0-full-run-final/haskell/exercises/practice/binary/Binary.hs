module Binary (toDecimal) where

toDecimal :: String -> Integer
toDecimal xs = toDecimal' xs 0
  where
    toDecimal' [] acc = acc
    toDecimal' (x:rest) acc
      | x == '0' = toDecimal' rest (acc * 2)
      | x == '1' = toDecimal' rest (acc * 2 + 1)
      | otherwise = 0
