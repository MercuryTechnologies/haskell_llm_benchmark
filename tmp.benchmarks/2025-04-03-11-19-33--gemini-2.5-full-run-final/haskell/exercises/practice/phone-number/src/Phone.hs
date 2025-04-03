module Phone (number) where

import Data.Char (isDigit)

-- | Cleans a phone number string according to NANP rules.
-- Removes punctuation and optional country code '1'.
-- Returns Just the 10-digit number if valid, otherwise Nothing.
number :: String -> Maybe String
number xs =
  let digits = filter isDigit xs
  in case length digits of
       10 -> validateNANP digits
       11 | head digits == '1' -> validateNANP (tail digits)
       _  -> Nothing

-- | Validates a 10-digit string against NANP rules (NXX-NXX-XXXX).
-- N must be 2-9.
validateNANP :: String -> Maybe String
validateNANP num =
  if isValidAreaCode (head num) && isValidExchangeCode (num !! 3)
  then Just num
  else Nothing
  where
    isValidAreaCode c = c >= '2' && c <= '9'
    isValidExchangeCode c = c >= '2' && c <= '9'
