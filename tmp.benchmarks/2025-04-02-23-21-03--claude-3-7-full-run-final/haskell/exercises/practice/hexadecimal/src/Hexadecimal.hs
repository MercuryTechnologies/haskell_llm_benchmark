module Hexadecimal (hexToInt) where

import Data.Char (toLower, isHexDigit)

-- | Convert a hexadecimal string to an integer
hexToInt :: String -> Integer
hexToInt [] = 0
hexToInt str = 
  if all isHexDigit str
    then foldl addHexDigit 0 str
    else 0
  where
    addHexDigit acc c = acc * 16 + hexDigitToInt (toLower c)
    
    hexDigitToInt :: Char -> Integer
    hexDigitToInt c
      | c >= '0' && c <= '9' = toInteger (fromEnum c - fromEnum '0')
      | c >= 'a' && c <= 'f' = toInteger (fromEnum c - fromEnum 'a' + 10)
      | otherwise = 0  -- This case shouldn't happen due to the isHexDigit check
