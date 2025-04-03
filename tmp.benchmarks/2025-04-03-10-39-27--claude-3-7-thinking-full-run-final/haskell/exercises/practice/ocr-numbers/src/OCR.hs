module OCR (convert) where

-- Main function to convert OCR input to numbers
convert :: String -> String
convert input = 
  let lines' = lines input
  in case parseInput lines' of
       Left err -> err
       Right digits -> digits

-- Parse input and handle errors
parseInput :: [String] -> Either String String
parseInput ls
  | null ls = Left "Error"
  | length ls `mod` 4 /= 0 = Left "Error"
  | otherwise = 
      let chunks = chunksOf 4 ls
          results = map parseChunk chunks
      in if any isLeft results
         then Left "Error"
         else Right (intercalate "," (map fromRight results))

-- Parse a single chunk (4 lines) into digits
parseChunk :: [String] -> Either String String
parseChunk [line1, line2, line3, _] = 
  if not (validLines [line1, line2, line3])
  then Left "Error"
  else Right (parseDigits line1 line2 line3)
parseChunk _ = Left "Error"

-- Check if lines have valid lengths
validLines :: [String] -> Bool
validLines ls = 
  all (\l -> length l `mod` 3 == 0) ls && 
  allSameLength ls && 
  not (any null ls)

-- Check if all strings have the same length
allSameLength :: [String] -> Bool
allSameLength [] = True
allSameLength [_] = True
allSameLength (x:y:xs) = length x == length y && allSameLength (y:xs)

-- Parse multiple digits from three lines
parseDigits :: String -> String -> String -> String
parseDigits line1 line2 line3 =
  let width = length line1
      positions = [0,3..width-3]
  in [parseDigit (getSlice i line1) (getSlice i line2) (getSlice i line3) | i <- positions]
  where
    getSlice i line = take 3 (drop i line)

-- Parse a single digit from its pattern
parseDigit :: String -> String -> String -> Char
parseDigit top middle bottom
  | pattern == zero   = '0'
  | pattern == one    = '1'
  | pattern == two    = '2'
  | pattern == three  = '3'
  | pattern == four   = '4'
  | pattern == five   = '5'
  | pattern == six    = '6'
  | pattern == seven  = '7'
  | pattern == eight  = '8'
  | pattern == nine   = '9'
  | otherwise         = '?'
  where 
    pattern = (top, middle, bottom)
    zero   = (" _ ", "| |", "|_|")
    one    = ("   ", "  |", "  |")
    two    = (" _ ", " _|", "|_ ")
    three  = (" _ ", " _|", " _|")
    four   = ("   ", "|_|", "  |")
    five   = (" _ ", "|_ ", " _|")
    six    = (" _ ", "|_ ", "|_|")
    seven  = (" _ ", "  |", "  |")
    eight  = (" _ ", "|_|", "|_|")
    nine   = (" _ ", "|_|", " _|")

-- Split list into chunks of n elements
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs =
  let (ys, zs) = splitAt n xs
  in ys : chunksOf n zs

-- Join strings with separator
intercalate :: String -> [String] -> String
intercalate _ [] = ""
intercalate _ [x] = x
intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs

-- Check if a value is a Left
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

-- Extract value from Right
fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight _ = error "Called fromRight on a Left value"
