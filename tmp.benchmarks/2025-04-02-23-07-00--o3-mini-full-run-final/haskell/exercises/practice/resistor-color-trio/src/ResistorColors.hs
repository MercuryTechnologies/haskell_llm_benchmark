module ResistorColors (Color(..), Resistor(..), label, ohms) where

import Numeric (showFFloat)

data Color =
    Black
  | Brown
  | Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Violet
  | Grey
  | White
  deriving (Show, Enum, Bounded)

newtype Resistor = Resistor { bands :: (Color, Color, Color) }
  deriving Show

ohms :: Resistor -> Int
ohms (Resistor (c1, c2, c3)) = (fromEnum c1 * 10 + fromEnum c2) * (10 ^ fromEnum c3)

label :: Resistor -> String
label r =
  let total = ohms r
      (divisor, unit)
         | total >= 10^9 = (10^9, "gigaohms")
         | total >= 10^6 = (10^6, "megaohms")
         | total >= 10^3 = (10^3, "kiloohms")
         | otherwise     = (1, "ohms")
  in if divisor == 1 then show total ++ " " ++ unit
     else let quotient = total `div` divisor
              remainder = total `mod` divisor
              value :: Double
              value = fromIntegral total / fromIntegral divisor
          in if remainder == 0
             then show quotient ++ " " ++ unit
             else showFFloat (Just 1) value (" " ++ unit)
