module ResistorColors (Color(..), Resistor(..), label, ohms) where

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

label :: Resistor -> String
label (Resistor (band1, band2, band3)) =
  let value = ohms (Resistor (band1, band2, band3)) in
  if value >= 1000000
  then show (value `div` 1000000) ++ " megaohms"
  else if value >= 1000
  then show (value `div` 1000) ++ " kiloohms"
  else show value ++ " ohms"

ohms :: Resistor -> Int
ohms (Resistor (band1, band2, band3)) =
  let digit1 = fromEnum band1
      digit2 = fromEnum band2
      zeros = fromEnum band3
  in (digit1 * 10 + digit2) * (10 ^ zeros)
