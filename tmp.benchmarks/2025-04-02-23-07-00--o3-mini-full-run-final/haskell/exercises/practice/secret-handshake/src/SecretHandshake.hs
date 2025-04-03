module SecretHandshake (handshake) where

import Data.Bits ((.&.))
import Data.List (reverse)

handshake :: Int -> [String]
handshake n =
    let n' = n .&. 31
        actions = [ ("wink", 1)
                  , ("double blink", 2)
                  , ("close your eyes", 4)
                  , ("jump", 8) ]
        hand = [ action | (action, bit) <- actions, n' .&. bit /= 0 ]
    in if n' .&. 16 /= 0 then reverse hand else hand
