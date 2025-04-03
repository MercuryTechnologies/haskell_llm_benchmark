module ArmstrongNumbers (armstrong) where

import Data.Char (digitToInt)

armstrong :: Integral a => a -> Bool
armstrong n =
  let nAbs        = toInteger (abs n)
      digits      = map digitToInt (show nAbs)
      numDigits   = length digits
      sumOfPowers = sum [ toInteger d ^ numDigits | d <- digits ]
  in sumOfPowers == nAbs
