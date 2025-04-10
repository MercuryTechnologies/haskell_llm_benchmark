module RotationalCipher (rotate) where

rotate :: Int -> String -> String
rotate shift = map (shiftChar shift)

shiftChar :: Int -> Char -> Char
shiftChar shift c
  | isLower c = shiftLetter shift 'a' c
  | isUpper c = shiftLetter shift 'A' c
  | otherwise = c

shiftLetter :: Int -> Char -> Char -> Char
shiftLetter shift base c = 
  let offset = ord c - ord base
      newOffset = (offset + shift) `mod` 26
  in chr (ord base + newOffset)

isLower :: Char -> Bool
isLower c = c >= 'a' && c <= 'z'

isUpper :: Char -> Bool
isUpper c = c >= 'A' && c <= 'Z'

ord :: Char -> Int
ord = fromEnum

chr :: Int -> Char
chr = toEnum
