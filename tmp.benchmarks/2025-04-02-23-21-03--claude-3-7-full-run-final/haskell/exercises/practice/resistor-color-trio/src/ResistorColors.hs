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

-- Calculate the resistance value in ohms
ohms :: Resistor -> Int
ohms (Resistor (tens, units, zeros)) = 
  (colorValue tens * 10 + colorValue units) * (10 ^ colorValue zeros)
  where
    colorValue :: Color -> Int
    colorValue = fromEnum

-- Format the resistance value with appropriate metric prefix
label :: Resistor -> String
label resistor =
  let value = ohms resistor
  in case () of
       _ | value < 1000 -> show value ++ " ohms"
         | value < 1000000 -> show (value `div` 1000) ++ " kiloohms"
         | value < 1000000000 -> show (value `div` 1000000) ++ " megaohms"
         | otherwise -> show (value `div` 1000000000) ++ " gigaohms"
