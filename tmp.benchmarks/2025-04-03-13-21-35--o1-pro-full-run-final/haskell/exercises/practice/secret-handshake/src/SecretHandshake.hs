module SecretHandshake (handshake) where

import Data.Bits (testBit)

handshake :: Int -> [String]
handshake n
  | testBit n 4 = reverse base
  | otherwise   = base
  where
    base = [ action
           | (bitIdx, action) <- zip [0..3]
                                  [ "wink"
                                  , "double blink"
                                  , "close your eyes"
                                  , "jump"
                                  ]
           , testBit n bitIdx
           ]
