module ArmstrongNumbers (armstrong) where

armstrong :: (Integral a, Show a) => a -> Bool
armstrong n =
  let digits = map (read . (:[])) (show n) :: [Int]
      len = length digits
      sumOfPowers = sum (map (\d -> d ^ len) digits)
  in fromIntegral sumOfPowers == n
