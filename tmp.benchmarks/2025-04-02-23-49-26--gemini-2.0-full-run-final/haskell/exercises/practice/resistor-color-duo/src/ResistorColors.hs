module ResistorColors (Color(..), value) where

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
  deriving (Eq, Show, Enum, Bounded)

colorCode :: Color -> Int
colorCode Black  = 0
colorCode Brown  = 1
colorCode Red    = 2
colorCode Orange = 3
colorCode Yellow = 4
colorCode Green  = 5
colorCode Blue   = 6
colorCode Violet = 7
colorCode Grey   = 8
colorCode White  = 9

value :: (Color, Color) -> Int
value (a, b) = (colorCode a) * 10 + (colorCode b)
