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
  deriving (Show, Eq, Enum, Bounded) -- Added Eq for comparisons if needed, though Enum is sufficient here

newtype Resistor = Resistor { bands :: (Color, Color, Color) }
  deriving Show

-- | Calculate the resistance value in ohms.
ohms :: Resistor -> Int
ohms (Resistor { bands = (c1, c2, c3) }) =
  let val1 = fromEnum c1
      val2 = fromEnum c2
      multiplier = fromEnum c3
      baseValue = val1 * 10 + val2
  in baseValue * (10 ^ multiplier)

-- | Generate a human-readable label for the resistor value.
label :: Resistor -> String
label resistor =
  let value = ohms resistor
      giga = 1000000000
      mega = 1000000
      kilo = 1000
  in case () of
       _ | value >= giga && value `rem` giga == 0 -> show (value `div` giga) ++ " gigaohms"
       _ | value >= mega && value `rem` mega == 0 -> show (value `div` mega) ++ " megaohms"
       _ | value >= kilo && value `rem` kilo == 0 -> show (value `div` kilo) ++ " kiloohms"
       _                                          -> show value ++ " ohms"

