module Binary (toDecimal) where

toDecimal :: String -> Int
toDecimal xs
  | any (`notElem` "01") xs = 0
  | otherwise = foldl (\acc digit -> acc * 2 + (if digit == '1' then 1 else 0)) 0 xs
