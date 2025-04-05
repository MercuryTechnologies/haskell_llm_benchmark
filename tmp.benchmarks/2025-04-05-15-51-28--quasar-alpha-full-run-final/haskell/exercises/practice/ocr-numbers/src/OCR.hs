module OCR (convert) where

import Data.List (transpose, intercalate)

-- Local implementation of chunksOf
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs =
  let (first, rest) = splitAt n xs
  in first : chunksOf n rest

-- Map of digit patterns to their string representation
digitMap :: [(String, Char)]
digitMap =
  [ (" _ | ||_|   ", '0')
  , ("     |  |   ", '1')
  , (" _  _||_    ", '2')
  , (" _  _| _|   ", '3')
  , ("   |_|  |   ", '4')
  , (" _ |_  _|   ", '5')
  , (" _ |_ |_|   ", '6')
  , (" _   |  |   ", '7')
  , (" _ |_||_|   ", '8')
  , (" _ |_| _|   ", '9')
  ]

-- Convert a single 3x4 digit block to a character
convertDigit :: [String] -> Either String Char
convertDigit [a,b,c,d]
  | length a == 3 && length b == 3 && length c == 3 && all (==3) (map length [a,b,c]) =
      case lookup (concat [a,b,c,d]) digitMap of
        Just ch -> Right ch
        Nothing -> Right '?'
  | otherwise = Left "invalid digit size"
convertDigit _ = Left "invalid digit size"

-- Convert a single line of digits (4 rows of text)
convertLine :: [String] -> Either String String
convertLine rows
  | length rows /= 4 = Left "invalid line height"
  | any ((`mod` 3) /= 0) (map length rows) = Left "invalid line width"
  | otherwise =
      let digitCols = map (chunksOf 3) rows
          digitBlocks = transpose digitCols
          digits = mapM convertDigit digitBlocks
      in digits

-- Convert the entire input string
convert :: String -> String
convert input =
  let allRows = lines input
  in if any ((`mod` 3) /= 0) (map length allRows)
        then error "invalid line width"
        else if (length allRows `mod` 4) /= 0
          then error "invalid line height"
          else
            let lineGroups = chunksOf 4 allRows
                convertedLines = map convertLine lineGroups
            in case sequence convertedLines of
                 Left err -> error err
                 Right strs -> intercalate "," strs
