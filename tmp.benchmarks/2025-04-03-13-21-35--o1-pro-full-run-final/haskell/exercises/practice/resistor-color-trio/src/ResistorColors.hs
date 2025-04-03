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

colorVal :: Color -> Int
colorVal Black  = 0
colorVal Brown  = 1
colorVal Red    = 2
colorVal Orange = 3
colorVal Yellow = 4
colorVal Green  = 5
colorVal Blue   = 6
colorVal Violet = 7
colorVal Grey   = 8
colorVal White  = 9

ohms :: Resistor -> Int
ohms (Resistor (c1, c2, c3)) =
  (10 * colorVal c1 + colorVal c2) * (10 ^ colorVal c3)

label :: Resistor -> String
label resistor =
  let val = ohms resistor
  in metricPrefix val

metricPrefix :: Int -> String
metricPrefix val
  | val >= 1000000000 && val `mod` 1000000000 == 0 = show (val `div` 1000000000) ++ " gigaohms"
  | val >= 1000000    && val `mod` 1000000 == 0    = show (val `div` 1000000)    ++ " megaohms"
  | val >= 1000       && val `mod` 1000 == 0       = show (val `div` 1000)       ++ " kiloohms"
  | otherwise                                     = show val                    ++ " ohms"
