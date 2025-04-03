module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance xs ys
    | length xs /= length ys = Nothing
    | otherwise = Just (calculateDistance xs ys)
    where
        calculateDistance :: String -> String -> Int
        calculateDistance [] [] = 0
        calculateDistance (x:xt) (y:yt)
            | x /= y    = 1 + calculateDistance xt yt
            | otherwise = calculateDistance xt yt
        -- This case should not happen due to the length check, but added for completeness
        calculateDistance _ _ = error "Input strings have different lengths after initial check, this should not happen."

