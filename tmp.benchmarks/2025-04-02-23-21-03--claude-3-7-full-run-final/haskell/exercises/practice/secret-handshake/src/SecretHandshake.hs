module SecretHandshake (handshake) where

handshake :: Int -> [String]
handshake n
  | n < 1 || n > 31 = []
  | otherwise = if reverse then reverse actions else actions
  where
    binary = toBinary n
    reverse = binary !! 4 == 1
    actions = filter (not . null) [
        if binary !! 0 == 1 then "wink" else "",
        if binary !! 1 == 1 then "double blink" else "",
        if binary !! 2 == 1 then "close your eyes" else "",
        if binary !! 3 == 1 then "jump" else ""
      ]
    
    -- Convert a number to a list of binary digits (0 or 1)
    -- Returns a list of 5 digits, padded with 0s if necessary
    toBinary :: Int -> [Int]
    toBinary num = take 5 $ map digit [0..4]
      where
        digit pos = if num `testBit` pos then 1 else 0
        testBit x p = (x `div` (2^p)) `mod` 2 == 1
