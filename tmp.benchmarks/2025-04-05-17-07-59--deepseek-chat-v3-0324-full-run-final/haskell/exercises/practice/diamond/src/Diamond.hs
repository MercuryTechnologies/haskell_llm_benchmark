module Diamond (diamond) where

diamond :: Char -> Maybe [String]
diamond c
  | c < 'A' || c > 'Z' = Nothing
  | otherwise = Just $ buildDiamond c

buildDiamond :: Char -> [String]
buildDiamond c = topHalf ++ bottomHalf
  where
    letters = ['A'..c]
    topHalf = [makeLine letter (length letters - 1) | letter <- letters]
    bottomHalf = reverse $ init topHalf
    makeLine current maxWidth =
      let currentPos = fromEnum current - fromEnum 'A'
          maxPos = fromEnum maxWidth
          outerSpaces = replicate (maxPos - currentPos) ' '
          innerSpaces = if current == 'A'
                        then ""
                        else replicate (2 * currentPos - 1) ' '
      in outerSpaces ++ [current] ++ innerSpaces ++ (if current == 'A' then "" else [current]) ++ outerSpaces
