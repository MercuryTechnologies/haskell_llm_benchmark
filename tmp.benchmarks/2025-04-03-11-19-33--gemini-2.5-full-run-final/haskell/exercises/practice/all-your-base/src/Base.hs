module Base (Error(..), rebase) where

import Data.List (foldl')

-- Define possible errors during base conversion
data Error a = InvalidInputBase    -- Input base is less than 2
             | InvalidOutputBase   -- Output base is less than 2
             | InvalidDigit a      -- An input digit is invalid for the input base
    deriving (Show, Eq)

-- | Convert a number represented as a list of digits in one base to another base.
-- Args:
--   inputBase: The base of the input digits (must be >= 2).
--   outputBase: The desired base for the output digits (must be >= 2).
--   inputDigits: A list of digits representing the number in inputBase.
-- Returns:
--   Either an Error or the list of digits in outputBase.
rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase inputDigits
  -- Validate the input base
  | inputBase < 2 = Left InvalidInputBase
  -- Validate the output base
  | outputBase < 2 = Left InvalidOutputBase
  -- If bases are valid, proceed with conversion
  | otherwise = do
      -- Convert input digits to an intermediate integer value.
      -- This step also validates the digits themselves.
      value <- toValue inputBase inputDigits
      -- Convert the integer value to the target output base digits.
      fromValue outputBase value

-- | Convert a list of digits in a given base to an integer value (base 10).
-- Performs validation checks on each digit.
toValue :: Integral a => a -> [a] -> Either (Error a) a
toValue base = foldl' step (Right 0)
  where
    -- The folding function: takes the current accumulated result (or error)
    -- and the next digit, returning the updated result (or error).
    -- REMOVED explicit type signature: step :: Integral a => Either (Error a) a -> a -> Either (Error a) a
    step (Right acc) digit
      -- Check if the digit is valid (0 <= digit < base)
      | 0 <= digit && digit < base = Right (acc * base + digit)
      -- If the digit is invalid, return an error
      | otherwise                  = Left (InvalidDigit digit)
    -- If an error has already occurred, propagate it
    step (Left err) _ = Left err

-- | Convert a non-negative integer value to a list of digits in a given base.
-- Assumes the base is valid (>= 2), as checked by the caller (rebase).
fromValue :: Integral a => a -> a -> Either (Error a) [a]
-- The number 0 is represented as [0] in any valid base.
fromValue _ 0 = Right [0]
-- For positive numbers, perform the conversion.
fromValue base n | n > 0 = Right . reverse $ generateDigits n
                 -- This case should ideally not be reached if input validation is correct,
                 -- as input digits are non-negative.
                 | otherwise = error "fromValue received a negative number. This should not happen."
  where
    -- Helper function to recursively generate digits in reverse order.
    -- REMOVED explicit type signature: generateDigits :: Integral a => a -> [a]
    generateDigits 0 = [] -- Base case: when the number becomes 0, stop.
    generateDigits x =
      let (quotient, remainder) = x `divMod` base -- Calculate quotient and remainder
      in remainder : generateDigits quotient       -- Prepend the remainder (digit) and recurse
