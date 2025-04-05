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
ohms (Resistor (c1, c2, multiplier)) =
  let firstDigit = colorValue c1
      secondDigit = colorValue c2
      zeros = colorValue multiplier
  in (firstDigit * 10 + secondDigit) * (10 ^ zeros)

label :: Resistor -> String
label resistor =
  let value = ohms resistor
  in if value >= 1000000
     then let mega = value `div` 1000000
          in show mega ++ " megaohms"
     else if value >= 1000
          then let kilo = value `div` 1000
               in show kilo ++ " kiloohms"
          else show value ++ " ohms"
