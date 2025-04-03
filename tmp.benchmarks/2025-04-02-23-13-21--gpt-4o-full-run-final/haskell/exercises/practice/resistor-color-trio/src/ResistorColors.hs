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
label resistor = 
    let resistance = ohms resistor
    in if resistance >= 1000000
       then show (resistance `div` 1000000) ++ " megaohms"
       else if resistance >= 1000
       then show (resistance `div` 1000) ++ " kiloohms"
       else show resistance ++ " ohms"

ohms :: Resistor -> Int
ohms (Resistor (band1, band2, band3)) =
    let value1 = fromEnum band1
        value2 = fromEnum band2
        multiplier = fromEnum band3
    in (value1 * 10 + value2) * (10 ^ multiplier)
