module Binary (toDecimal) where

-- Function to convert binary string to decimal
toDecimal :: String -> Maybe Int
toDecimal xs
  | isValidBinary xs = Just (binaryToDecimal xs)
  | otherwise        = Nothing

-- Helper function to check if a string is a valid binary number
isValidBinary :: String -> Bool
isValidBinary = all (`elem` "01")

-- Helper function to convert a valid binary string to decimal
binaryToDecimal :: String -> Int
binaryToDecimal = foldr (\c acc -> acc * 2 + digitToInt c) 0
  where
    digitToInt :: Char -> Int
    digitToInt '0' = 0
    digitToInt '1' = 1
    digitToInt _   = error "Invalid binary digit"  -- Add a catch-all pattern
