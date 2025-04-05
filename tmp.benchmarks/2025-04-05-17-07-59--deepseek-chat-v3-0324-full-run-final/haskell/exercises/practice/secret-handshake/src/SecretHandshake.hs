module SecretHandshake (handshake) where

import Data.Bits ((.&.))

handshake :: Int -> [String]
handshake n 
    | n < 1 || n > 31 = []
    | otherwise = 
        let actions = [ (1, "wink")
                      , (2, "double blink")
                      , (4, "close your eyes")
                      , (8, "jump")
                      ]
            reverseFlag = 16
            baseActions = map snd $ filter (\(mask, _) -> n .&. mask /= 0) actions
        in if n .&. reverseFlag /= 0
           then reverse baseActions
           else baseActions
