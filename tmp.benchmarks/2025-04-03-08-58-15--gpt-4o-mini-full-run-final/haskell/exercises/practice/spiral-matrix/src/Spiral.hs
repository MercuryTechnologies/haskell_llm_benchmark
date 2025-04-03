module Spiral (spiral) where

spiral :: Int -> [[Int]]
spiral size = generateSpiral size

generateSpiral :: Int -> [[Int]]
generateSpiral n = go 1 0 0 n []
  where
    go num x y size acc
      | size <= 0 = acc
      | otherwise = let (top, rest) = fillTop num x y size
                        (right, rest') = fillRight (last top) (x + size - 1) (y + 1) (size - 1)
                        (bottom, rest'') = fillBottom (last right) (x + size - 1) (y + size - 1) (size - 1)
                        (left, finalAcc) = fillLeft (last bottom) (x) (y + size - 1) (size - 1)
                    in go (last left + 1) (x + 1) (y + 1) (size - 2) (acc ++ [top] ++ right ++ bottom ++ left)

    fillTop num x y size = (take size [num..num + size - 1], num + size)
    fillRight num x y size = (take size [num, num + 1..num + size - 1], num + size)
    fillBottom num x y size = (reverse (take size [num, num - 1..num - size + 1]), num - size + 1)
    fillLeft num x y size = (reverse (take size [num, num - 1..num - size + 1]), num - size + 1)
