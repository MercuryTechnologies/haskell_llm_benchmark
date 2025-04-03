module Series (Error(..), largestProduct) where

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

largestProduct :: Int -> String -> Either Error Integer
largestProduct size digits
    | size < 0 = Left InvalidSpan
    | size > length digits = Left InvalidSpan
    | otherwise =
        case traverse parseDigit digits of
          Left err -> Left err
          Right parsed ->
              if size == 0
                then Right 1
                else Right $ maximum [ product (take size (drop i parsed))
                                     | i <- [0 .. length parsed - size]
                                     ]
  where
    parseDigit c
      | '0' <= c && c <= '9' = Right (toInteger (fromEnum c - fromEnum '0'))
      | otherwise           = Left (InvalidDigit c)
