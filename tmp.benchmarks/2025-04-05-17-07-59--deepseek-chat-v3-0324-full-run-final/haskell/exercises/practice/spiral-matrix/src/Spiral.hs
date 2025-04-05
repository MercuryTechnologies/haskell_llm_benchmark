module Spiral (spiral) where

spiral :: Int -> [[Int]]
spiral size = buildSpiral size 1
  where
    buildSpiral 0 _ = []
    buildSpiral 1 n = [[n]]
    buildSpiral 2 n = [[n, n+1], [n+3, n+2]]
    buildSpiral s n =
      [ [n .. n + s - 1] ] ++
      map (\x -> [last (head res) + x]) [1 .. s - 2] ++
      [ reverse [n + s * 3 - 3 - x | x <- [1 .. s]] ] ++
      map (\x -> [res !! 1 !! 0 - x]) [1 .. s - 2]
      where
        res = buildSpiral (s - 2) (n + s * 4 - 4)
