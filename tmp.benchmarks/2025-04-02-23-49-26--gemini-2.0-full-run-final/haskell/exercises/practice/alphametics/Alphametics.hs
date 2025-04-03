module Alphametics (solve) where

import Data.List (nub, permutations)
import Data.Maybe (fromMaybe)

solve :: String -> Maybe [(Char, Int)]
solve puzzle = solveHelper (nub $ filter isLetter puzzle)
  where
    isLetter c = c `elem` ['A'..'Z']
    equation = words $ map (\c -> if c == '=' then '+' else c) puzzle
    operands = init equation
    result = last equation
    firstChars = nub $ map head $ operands ++ [result]

    solveHelper :: [Char] -> Maybe [(Char, Int)]
    solveHelper letters =
      let
        nums = [0..9]
        possibleAssignments = permutations nums
        isValidAssignment assignment =
          and $
            zipWith
              (\char num ->
                if char `elem` firstChars
                  then num /= 0
                  else True
              )
              letters
              assignment
        getValue word assignment =
          foldl (\acc c -> acc * 10 + fromIntegral (assignment !! (letterIndex c letters))) 0 word
        letterIndex c letters = fromMaybe (error "letter not found") (elemIndex c letters)

        elemIndex :: Eq a => a -> [a] -> Maybe Int
        elemIndex x xs = elemIndex' x xs 0
          where
            elemIndex' _ [] _ = Nothing
            elemIndex' target (y:ys) index =
              if target == y
                then Just index
                else elemIndex' target ys (index + 1)

        checkEquation assignment =
          let
            operandValues = map (\operand -> getValue operand assignment) operands
            resultValue = getValue result assignment
          in
            sum operandValues == resultValue
      in
        getFirstJust $
          map
            (\assignment ->
              if isValidAssignment assignment && checkEquation assignment
                then Just (zip letters $ map fromIntegral assignment)
                else Nothing
            )
            possibleAssignments

    getFirstJust :: [Maybe a] -> Maybe a
    getFirstJust [] = Nothing
    getFirstJust (Just x : _) = Just x
    getFirstJust (Nothing : xs) = getFirstJust xs
