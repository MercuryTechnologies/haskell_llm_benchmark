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

-- | Calculate the numeric value of the first two colors in a resistor band.
-- The first color represents the tens digit, and the second color represents
-- the units digit.
value :: (Color, Color) -> Int
value (a, b) = fromEnum a * 10 + fromEnum b
