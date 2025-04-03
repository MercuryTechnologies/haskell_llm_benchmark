module SecretHandshake (handshake) where

handshake :: Int -> [String]
handshake n = reverse actions
  where
    actions = concatMap (\(bit, action) -> if bit == '1' then [action] else []) (zip binary actionsList)
    binary = take 5 (reverse (toBinary n))
    actionsList = ["wink", "double blink", "close your eyes", "jump", "reverse actions"]

toBinary :: Int -> String
toBinary 0 = "0"
toBinary n = toBinary (n `div` 2) ++ show (n `mod` 2)
