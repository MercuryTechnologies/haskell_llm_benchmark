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

colorToValue :: Color -> Int
colorToValue color = fromEnum color

ohms :: Resistor -> Int
ohms (Resistor (c1, c2, c3)) = (colorToValue c1 * 10 + colorToValue c2) * (10 ^ colorToValue c3)

label :: Resistor -> String
label resistor =
  let resistance = ohms resistor
  in if resistance >= 1000000
     then show (resistance `div` 1000000) ++ " megaohms"
     else if resistance >= 1000
          then show (resistance `div` 1000) ++ " kiloohms"
          else show resistance ++ " ohms"
