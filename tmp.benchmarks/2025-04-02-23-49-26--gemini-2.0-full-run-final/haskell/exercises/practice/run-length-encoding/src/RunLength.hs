module RunLength (decode, encode) where

decode :: String -> String
decode "" = ""
decode encodedText = decode' encodedText ""
  where
    decode' :: String -> String -> String
    decode' "" acc = acc
    decode' (x:xs) acc
      | isDigit x =
        let (numStr, rest) = span isDigit (x:xs)
            num = read numStr :: Int
            char = head rest
            newAcc = acc ++ replicate num char
        in decode' (tail rest) newAcc
      | otherwise = decode' xs (acc ++ [x])

encode :: String -> String
encode "" = ""
encode text = encode' text "" 1
  where
    encode' :: String -> String -> Int -> String
    encode' "" acc _ = acc
    encode' (x:xs) acc count
      | null xs = acc ++ encodeChar count x
      | x == head xs = encode' xs acc (count + 1)
      | otherwise = encode' xs (acc ++ encodeChar count x) 1

encodeChar :: Int -> Char -> String
encodeChar 1 char = [char]
encodeChar count char = show count ++ [char]

isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'
