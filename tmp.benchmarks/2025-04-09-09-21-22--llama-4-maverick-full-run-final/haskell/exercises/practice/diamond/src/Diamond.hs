module Diamond (diamond) where

diamond :: Char -> Maybe [String]
diamond c
  | c < 'A' || c > 'Z' = Nothing
  | otherwise = Just (generateDiamond c)

generateDiamond :: Char -> [String]
generateDiamond c = topHalf ++ tail (reverse topHalf)
  where
    width = ord c - ord 'A' + 1
    topHalf = [generateLine letter width | letter <- ['A'..c]]

generateLine :: Char -> Int -> String
generateLine letter width = 
  let
    outerSpaces = width - ord letter + ord 'A' - 1
    innerSpaces = 2 * (ord letter - ord 'A') - 1
  in
    replicate outerSpaces ' ' ++ [letter] ++ 
    (if innerSpaces >= 0 then replicate innerSpaces ' ' ++ [letter] else "") ++ 
    replicate outerSpaces ' '

ord :: Char -> Int
ord c = fromEnum c
