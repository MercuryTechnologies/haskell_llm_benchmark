module Spiral (spiral) where

spiral :: Int -> [[Int]]
spiral size = buildSpiral size 1
  where
    buildSpiral 0 _ = []
    buildSpiral 1 start = [[start]]
    buildSpiral n start =
      let
        -- Top row
        top = [start .. start + n - 1]
        -- Right column
        right = [start + n .. start + 2 * n - 2]
        -- Bottom row (reversed)
        bottom = reverse [start + 2 * n - 1 .. start + 3 * n - 3]
        -- Left column (reversed)
        left = reverse [start + 3 * n - 2 .. start + 4 * n - 4]
        -- Inner spiral
        inner = buildSpiral (n - 2) (start + 4 * n - 4)
        -- Add left and right columns to inner spiral rows
        innerWithBorders =
          if null inner
            then map (\(l, r) -> [l, r]) (zip left right)
            else zipWith (\l r -> l ++ r) (map (\x -> [x]) left) (zipWith (++) inner (map (\x -> [x]) right))
      in
        [top] ++ innerWithBorders ++ [bottom]
