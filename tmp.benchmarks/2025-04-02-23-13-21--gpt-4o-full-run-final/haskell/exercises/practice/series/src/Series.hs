module Series (slices) where

slices :: Int -> String -> [[Int]]
slices n xs
  | n > length xs = []
  | otherwise = [map (read . (:[])) (take n (drop i xs)) | i <- [0..(length xs - n)]]
