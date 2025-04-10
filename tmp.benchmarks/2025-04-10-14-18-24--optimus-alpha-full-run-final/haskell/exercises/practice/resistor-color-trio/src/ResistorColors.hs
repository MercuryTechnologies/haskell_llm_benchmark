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

colorValue :: Color -> Int
colorValue = fromEnum

ohms :: Resistor -> Int
ohms (Resistor (c1, c2, c3)) =
  let d1 = colorValue c1
      d2 = colorValue c2
      zeros = colorValue c3
  in (d1 * 10 + d2) * (10 ^ zeros)

label :: Resistor -> String
label r =
  let value = ohms r
  in case () of
    _ | value >= 1000000 && value `mod` 1000000 == 0 ->
          show (value `div` 1000000) ++ " megaohms"
      | value >= 1000 && value `mod` 1000 == 0 ->
          show (value `div` 1000) ++ " kiloohms"
      | otherwise ->
          show value ++ " ohms"
