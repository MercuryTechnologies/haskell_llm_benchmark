module Spiral (spiral) where

spiral :: Int -> [[Int]]
spiral 0 = []
spiral 1 = [[1]]
spiral 2 = [[1, 2], [4, 3]]  -- Special case for 2x2 matrix
spiral size = spiralHelper size 1 []

-- Helper function that builds the spiral matrix
-- Parameters: size of matrix, starting number, accumulated result
spiralHelper :: Int -> Int -> [[Int]] -> [[Int]]
spiralHelper size start acc
  | size <= 0 = acc
  | otherwise = spiralHelper (size - 2) (start + 4 * size - 4) (buildSpiral size start acc)

-- Builds a single layer of the spiral
buildSpiral :: Int -> Int -> [[Int]] -> [[Int]]
buildSpiral size start acc
  | size == 1 = [[start]] ++ acc  -- Special case for 1x1 matrix
  | otherwise = 
      let top = [start..(start + size - 2)] ++ [start + size - 1]
          right = take (size - 2) [start + size..(start + 2 * size - 3)]
          bottom = reverse [start + 2 * size - 2..(start + 3 * size - 4)]
          left = reverse [start + 3 * size - 3..(start + 4 * size - 5)]
          
          -- Create the outer layer
          outerLayer = [top] ++ 
                       zipWith (\r v -> [r] ++ replicate (size - 2) 0 ++ [v]) left right ++
                       [[start + 3 * size - 3] ++ bottom]
          
          -- Insert the inner spiral into the middle of the outer layer
          insertInner [] = outerLayer
          insertInner inner = 
            let (before, after) = splitAt 1 outerLayer
                (lastPart, _) = splitAt (length outerLayer - 1) after
                middle = zipWith (\old new -> 
                           let (l, r) = splitAt 1 old
                               (_, e) = splitAt (length old - 1) r
                           in l ++ new ++ e) (init lastPart) inner
            in before ++ middle ++ [lastPart !! (length lastPart - 1)]
      in 
        if null acc 
        then outerLayer
        else insertInner acc
