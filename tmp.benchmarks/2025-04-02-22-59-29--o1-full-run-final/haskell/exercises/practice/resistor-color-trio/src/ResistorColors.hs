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

valueOf :: Color -> Int
valueOf Black  = 0
valueOf Brown  = 1
valueOf Red    = 2
valueOf Orange = 3
valueOf Yellow = 4
valueOf Green  = 5
valueOf Blue   = 6
valueOf Violet = 7
valueOf Grey   = 8
valueOf White  = 9

ohms :: Resistor -> Int
ohms (Resistor (c1, c2, c3)) =
  let baseVal = 10 * valueOf c1 + valueOf c2
      zeros   = valueOf c3
  in baseVal * (10 ^ zeros)

label :: Resistor -> String
label resistor =
  let val = ohms resistor
  in if val >= 1000000
     then show (val `div` 1000000) ++ " megaohms"
     else if val >= 1000
          then show (val `div` 1000) ++ " kiloohms"
          else show val ++ " ohms"
