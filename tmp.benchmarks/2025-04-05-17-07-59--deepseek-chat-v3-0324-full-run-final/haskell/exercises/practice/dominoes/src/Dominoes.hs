module Dominoes (chain) where

chain :: [(Int, Int)] -> Maybe [(Int, Int)]
chain [] = Just []
chain [(a, b)] = if a == b then Just [(a, b)] else Nothing
chain dominoes = 
    case findChain dominoes of
        [] -> Nothing
        x:xs -> Just (x:xs)

findChain :: [(Int, Int)] -> [(Int, Int)]
findChain [] = []
findChain dominoes = 
    let allChains = concatMap (buildChain []) (permutations dominoes)
    in if null allChains then [] else head allChains

buildChain :: [(Int, Int)] -> [(Int, Int)] -> [[(Int, Int)]]
buildChain current [] 
    | isValidChain current = [current]
    | otherwise = []
buildChain current (d:ds) =
    case current of
        [] -> buildChain [d] ds
        (lastA, lastB):_ -> 
            let matches = [(x,y) | (x,y) <- [d, swap d], lastB == x]
            in concatMap (\match -> buildChain (current ++ [match]) (delete match (d:ds))) matches

isValidChain :: [(Int, Int)] -> Bool
isValidChain chain =
    case chain of
        [] -> True
        [(a,b)] -> a == b
        (firstA, firstB):rest -> 
            let (lastA, lastB) = last rest
            in firstA == lastB

swap :: (Int, Int) -> (Int, Int)
swap (a, b) = (b, a)

delete :: Eq a => a -> [a] -> [a]
delete x [] = []
delete x (y:ys) 
    | x == y = ys
    | otherwise = y : delete x ys

permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations xs = [x:ys | x <- xs, ys <- permutations (delete x xs)]
