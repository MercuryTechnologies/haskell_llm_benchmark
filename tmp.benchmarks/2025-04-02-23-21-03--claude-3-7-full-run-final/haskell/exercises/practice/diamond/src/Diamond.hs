module Diamond (diamond) where

diamond :: Char -> Maybe [String]
diamond char
  | char >= 'A' && char <= 'Z' = Just $ makeDiamond char
  | char >= 'a' && char <= 'z' = Just $ makeDiamond (toUpper char)
  | otherwise = Nothing
  where
    toUpper c
      | c >= 'a' && c <= 'z' = toEnum (fromEnum c - 32)
      | otherwise = c

    makeDiamond c = topHalf ++ tail bottomHalf
      where
        topHalf = [makeLine i | i <- ['A'..c]]
        bottomHalf = reverse topHalf
        width = 2 * (fromEnum c - fromEnum 'A') + 1
        
        makeLine ch = leadingSpaces ++ letters ++ trailingSpaces
          where
            position = fromEnum ch - fromEnum 'A'
            leadingSpaces = replicate position ' '
            
            letters
              | ch == 'A' = "A"
              | otherwise = ch : middleSpaces ++ [ch]
              
            middleSpaces = replicate (width - 2 * position - 2) ' '
            trailingSpaces = replicate (width - position - length letters) ' '
