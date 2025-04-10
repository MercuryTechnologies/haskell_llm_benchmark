module SecretHandshake (handshake) where

handshake :: Int -> [String]
handshake n = 
    let binary = take 5 $ reverse $ padWithZeros (bin n)
        actions = [ if binary !! 0 == 1 then "wink" else "",
                    if binary !! 1 == 1 then "double blink" else "",
                    if binary !! 2 == 1 then "close your eyes" else "",
                    if binary !! 3 == 1 then "jump" else "" ]
        reversed = binary !! 4 == 1
        result = filter (/= "") actions
    in if reversed then reverse result else result

bin :: Int -> [Int]
bin 0 = []
bin n = n `mod` 2 : bin (n `div` 2)

padWithZeros :: [Int] -> [Int]
padWithZeros xs = take 5 (xs ++ repeat 0)
