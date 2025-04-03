module SecretHandshake (handshake) where

handshake :: Int -> [String]
handshake n = 
    let actions = zip [0..] ["wink", "double blink", "close your eyes", "jump"]
        binaryActions = [action | (i, action) <- actions, n `div` (2^i) `mod` 2 == 1]
        shouldReverse = n `div` 16 `mod` 2 == 1
    in if shouldReverse then reverse binaryActions else binaryActions
