module IsbnVerifier (isbn) where

isbn :: String -> Bool
isbn s = case preprocess s of
          Just s' -> validateIsbn s'
          Nothing -> False

preprocess :: String -> Maybe String
preprocess s = let s' = filter (/= '-') s
               in if length s' == 10 then Just s' else Nothing

validateIsbn :: String -> Bool
validateIsbn s = case mapM toDigit s of
                  Just digits -> sum (zipWith (*) [10, 9 .. 1] digits) `mod` 11 == 0
                  Nothing     -> False

toDigit :: Char -> Maybe Int
toDigit 'X' = Just 10
toDigit c | c >= '0' && c <= '9' = Just (read [c])
          | otherwise = Nothing
