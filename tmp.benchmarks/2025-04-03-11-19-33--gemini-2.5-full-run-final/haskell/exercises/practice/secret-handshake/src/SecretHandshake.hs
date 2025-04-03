module SecretHandshake (handshake) where

import Data.Bits (testBit)

-- List of actions corresponding to bit positions 0 through 3
actions :: [(Int, String)]
actions =
  [ (0, "wink")
  , (1, "double blink")
  , (2, "close your eyes")
  , (3, "jump")
  ]

-- | Convert a number into a secret handshake sequence.
handshake :: Int -> [String]
handshake n =
  -- Generate the initial list of actions based on bits 0-3
  let initialActions = [action | (bit, action) <- actions, testBit n bit]
      -- Check if the 5th bit (position 4) is set, indicating reversal
      shouldReverse = testBit n 4
  -- Return the actions, reversed if necessary
  in if shouldReverse
     then reverse initialActions
     else initialActions
