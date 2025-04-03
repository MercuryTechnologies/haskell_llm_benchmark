module WordProblem (answer) where

answer :: String -> Maybe Integer
answer problem =
  let trimmed = trim problem
  in case stripPrefix "What is " trimmed of
       Nothing -> Nothing
       Just withoutPrefix ->
         if null withoutPrefix || last withoutPrefix /= '?'
           then Nothing
           else let content = init withoutPrefix
                    tokens = words content
                 in case tokens of
                      [] -> Nothing
                      (x:xs) -> do
                        initial <- readInteger x
                        evalTokens initial xs

  where
    evalTokens :: Integer -> [String] -> Maybe Integer
    evalTokens acc [] = Just acc
    evalTokens acc ("plus":num:rest) = do
      n <- readInteger num
      evalTokens (acc + n) rest
    evalTokens acc ("minus":num:rest) = do
      n <- readInteger num
      evalTokens (acc - n) rest
    evalTokens acc ("multiplied":"by":num:rest) = do
      n <- readInteger num
      evalTokens (acc * n) rest
    evalTokens acc ("divided":"by":num:rest) = do
      n <- readInteger num
      if n == 0 then Nothing else evalTokens (acc `div` n) rest
    evalTokens _ _ = Nothing

    readInteger :: String -> Maybe Integer
    readInteger s =
      case reads s of
        [(n, "")] -> Just n
        _         -> Nothing

    trim :: String -> String
    trim = f . f
      where f = reverse . dropWhile (== ' ')

    stripPrefix :: String -> String -> Maybe String
    stripPrefix [] ys = Just ys
    stripPrefix _ [] = Nothing
    stripPrefix (x:xs) (y:ys)
      | x == y    = stripPrefix xs ys
      | otherwise = Nothing
