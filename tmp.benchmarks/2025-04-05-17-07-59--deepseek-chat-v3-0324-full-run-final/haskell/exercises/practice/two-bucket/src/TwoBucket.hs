module TwoBucket (measure) where

data Bucket = Bucket1 | Bucket2 deriving (Eq, Show)

measure :: (Int, Int) -> Int -> Maybe (Int, (Int, Int))
measure (cap1, cap2) target = 
    case solve (cap1, 0) 1 [(cap1,0)] of
        Just (steps, (b1, b2)) -> Just (steps, if b1 == target then (b1, b2) else (b2, b1))
        Nothing -> Nothing
  where
    solve (b1, b2) steps visited
        | b1 == target = Just (steps, (b1, b2))
        | b2 == target = Just (steps, (b1, b2))
        | otherwise =
            let nextStates = filter (\(nb1, nb2) -> (nb1, nb2) `notElem` visited && 
                                  not (nb1 == 0 && nb2 == cap2)) $
                    [ fill1 (b1, b2)
                    , fill2 (b1, b2)
                    , empty1 (b1, b2)
                    , empty2 (b1, b2)
                    , pour12 (b1, b2)
                    , pour21 (b1, b2)
                    ]
            in case nextStates of
                [] -> Nothing
                _ -> foldl (\acc (nb1, nb2) -> 
                        case acc of
                            Just x -> Just x
                            Nothing -> solve (nb1, nb2) (steps + 1) ((nb1, nb2):visited)
                    ) Nothing nextStates

    fill1 (_, b2) = (cap1, b2)
    fill2 (b1, _) = (b1, cap2)
    empty1 (_, b2) = (0, b2)
    empty2 (b1, _) = (b1, 0)
    pour12 (b1, b2) = 
        let amount = min b1 (cap2 - b2)
        in (b1 - amount, b2 + amount)
    pour21 (b1, b2) = 
        let amount = min b2 (cap1 - b1)
        in (b1 + amount, b2 - amount)
