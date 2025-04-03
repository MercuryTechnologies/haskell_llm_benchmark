module Transpose (transpose) where

transpose :: [String] -> [String]
transpose xs
  | null xs = []
  | otherwise =
      let lens = map length xs
          maxLen = maximum lens
          indexes = [0 .. length xs - 1]

          -- For each column c, find the highest-index row that has a character
          maxIndex c = maximum [ i | i <- indexes, lens !! i > c ]

      in [ [ if lens !! i > c
             then (xs !! i) !! c
             else ' '
           | i <- [0 .. maxIndex c]
           ]
         | c <- [0 .. maxLen - 1]
         ]
