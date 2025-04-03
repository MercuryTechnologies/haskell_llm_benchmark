module Raindrops (convert) where

convert :: Int -> String
convert n =
  let
    result =
      (if n `mod` 3 == 0 then "Pling" else "")
      ++ (if n `mod` 5 == 0 then "Plang" else "")
      ++ (if n `mod` 7 == 0 then "Plong" else "")
  in
    if null result
       then show n
       else result
