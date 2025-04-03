module ArmstrongNumbers (armstrong) where

import Data.Char (digitToInt)
import Data.List (foldl')
import Numeric.Natural
import IntegerLogarithms (integerLog10)

armstrong :: Integral a => a -> Bool
armstrong n
  | n < 0     = False
  | otherwise =
    let
      numStr = show n
      numDigits = length numStr
      digits = map digitToInt numStr
      poweredDigits = map (\x -> x ^ numDigits) digits
      sumOfPoweredDigits = foldl' (+) 0 poweredDigits
    in
      sumOfPoweredDigits == n
