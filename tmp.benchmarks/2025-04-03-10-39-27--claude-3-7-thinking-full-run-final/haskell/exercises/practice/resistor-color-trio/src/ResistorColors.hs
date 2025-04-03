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

label :: Resistor -> String
label resistor = 
  let ohmValue = ohms resistor
      (value, unit) = formatOhms ohmValue
  in value ++ " " ++ unit

formatOhms :: Int -> (String, String)
formatOhms n
  | n < 1000 = (show n, "ohms")
  | n < 1000000 = (show (n `div` 1000), "kiloohms")
  | n < 1000000000 = (show (n `div` 1000000), "megaohms")
  | otherwise = (show (n `div` 1000000000), "gigaohms")

ohms :: Resistor -> Int
ohms (Resistor (first, second, third)) =
  let baseValue = colorValue first * 10 + colorValue second
      multiplier = 10 ^ colorValue third
  in baseValue * multiplier
