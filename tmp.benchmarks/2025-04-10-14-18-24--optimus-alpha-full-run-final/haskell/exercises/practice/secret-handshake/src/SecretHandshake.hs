module SecretHandshake (handshake) where

handshake :: Int -> [String]
handshake n = 
    let actions = [ (0, "wink")
                  , (1, "double blink")
                  , (2, "close your eyes")
                  , (3, "jump")
                  ]
        isSet bit = (n `div` (2^bit)) `mod` 2 == 1
        baseActions = [action | (bit, action) <- actions, isSet bit]
        reversed = isSet 4
    in if reversed then reverse baseActions else baseActions
