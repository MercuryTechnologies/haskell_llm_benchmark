module Roman (numerals) where

-- Mapping of integer values to their Roman numeral representations.
mapping :: [(Integer, String)]
mapping =
  [ (1000, "M")
  , (900 , "CM")
  , (500 , "D")
  , (400 , "CD")
  , (100 , "C")
  , (90  , "XC")
  , (50  , "L")
  , (40  , "XL")
  , (10  , "X")
  , (9   , "IX")
  , (5   , "V")
  , (4   , "IV")
  , (1   , "I")
  ]

-- Helper function that recursively constructs the Roman numeral.
romanHelper :: Integer -> String
romanHelper 0 = ""
romanHelper n =
  let (value, symbol) = head (filter (\(val, _) -> val <= n) mapping)
   in symbol ++ romanHelper (n - value)

-- Convert the given Integer to a Roman numeral, or Nothing if out of range.
numerals :: Integer -> Maybe String
numerals n
  | n < 1     = Nothing
  | n > 3999  = Nothing
  | otherwise = Just (romanHelper n)
