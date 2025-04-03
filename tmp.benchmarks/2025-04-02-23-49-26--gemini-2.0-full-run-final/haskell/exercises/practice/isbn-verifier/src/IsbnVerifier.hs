module IsbnVerifier (isbn) where

isbn :: String -> Bool
isbn str =
  let cleanedStr = filter (/= '-') str
      len = length cleanedStr
  in
  if len /= 10
    then False
    else
      let digits = map charToDigit cleanedStr
          isValid = all (/= -1) digits
          lastIsX = last cleanedStr == 'X'
          xIsValid = lastIsX && all (/= 'X') (init cleanedStr)
      in
      if not isValid || (lastIsX && not xIsValid)
        then False
        else
          let weightedSum = sum $ zipWith (*) [10,9..1] (init digits ++ [lastDigit (last cleanedStr)])
          in weightedSum `mod` 11 == 0

charToDigit :: Char -> Int
charToDigit 'X' = 10
charToDigit c | '0' <= c && c <= '9' = read [c]
charToDigit _ = -1

lastDigit :: Char -> Int
lastDigit 'X' = 10
lastDigit c = read [c]
