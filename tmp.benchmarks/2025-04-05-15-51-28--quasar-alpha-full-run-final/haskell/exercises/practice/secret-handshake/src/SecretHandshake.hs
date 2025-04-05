module SecretHandshake (handshake) where

import Data.Bits ((.&.))

handshake :: Int -> [String]
handshake n = 
    let actions = [ (1, "wink")
                  , (2, "double blink")
                  , (4, "close your eyes")
                  , (8, "jump")
                  ]
        selected = [ action | (bit, action) <- actions, n .&. bit /= 0 ]
        reversed = n .&. 16 /= 0
    in if reversed then reverse selected else selected
