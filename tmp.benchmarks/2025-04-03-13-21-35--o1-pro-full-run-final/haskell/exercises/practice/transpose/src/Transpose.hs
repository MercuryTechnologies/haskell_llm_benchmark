module Transpose (transpose) where

transpose :: [String] -> [String]
transpose [] = []
transpose xs =
  let
    maxLen = maximum (map length xs)
    -- Mark real characters as Right c, and padding as Left ' '
    col c = [ if length row > c then Right (row !! c) else Left ' '
            | row <- xs
            ]
    
    -- Remove trailing padding (Left ' ') from the end
    removeTrailingPads = reverse . dropWhile isPad . reverse
      where
        isPad (Left ' ') = True
        isPad (Right _)  = False
    
    -- Convert Either Char Char to Char
    fromEither = either id id
  in
    [ map fromEither (removeTrailingPads (col i))
    | i <- [0 .. maxLen - 1]
    ]
