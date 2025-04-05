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
label resistor = formatOhms (ohms resistor)

ohms :: Resistor -> Int
ohms (Resistor (c1, c2, c3)) = 
  let mainValue = colorValue c1 * 10 + colorValue c2
      zeros = colorValue c3
  in mainValue * (10 ^ zeros)

formatOhms :: Int -> String
formatOhms n
  | n >= 1000000000 = show (n `div` 1000000000) ++ " gigaohms"
  | n >= 1000000 = show (n `div` 1000000) ++ " megaohms"
  | n >= 1000 = show (n `div` 1000) ++ " kiloohms"
  | otherwise = show n ++ " ohms"
