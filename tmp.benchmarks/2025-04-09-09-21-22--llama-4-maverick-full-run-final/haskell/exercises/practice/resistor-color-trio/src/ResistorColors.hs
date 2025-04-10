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
ohms (Resistor (c1, c2, c3)) = 
    let value = fromEnum c1 * 10 + fromEnum c2
        multiplier = 10 ^ fromEnum c3
    in value * multiplier
