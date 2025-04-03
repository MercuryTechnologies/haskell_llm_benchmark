module Palindromes (largestPalindrome, smallestPalindrome) where

largestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
largestPalindrome minFactor maxFactor =
    let validFactors = [minFactor .. maxFactor]
        palList = [ (p, (i,j))
                  | i <- validFactors
                  , j <- validFactors
                  , let p = i * j
                  , show p == reverse (show p)
                  ]
    in if null palList
       then Nothing
       else let maxP = maximum (map fst palList)
                pairs = [ (i, j) | (p, (i, j)) <- palList, p == maxP ]
            in Just (maxP, pairs)

smallestPalindrome :: Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
smallestPalindrome minFactor maxFactor =
    let validFactors = [minFactor .. maxFactor]
        palList = [ (p, (i,j))
                  | i <- validFactors
                  , j <- validFactors
                  , let p = i * j
                  , show p == reverse (show p)
                  ]
    in if null palList
       then Nothing
       else let minP = minimum (map fst palList)
                pairs = [ (i, j) | (p, (i, j)) <- palList, p == minP ]
            in Just (minP, pairs)
