module Sublist (sublist) where

import Data.List (isInfixOf)

sublist :: Eq a => [a] -> [a] -> Maybe Ordering
sublist a b
  | a == b         = Just EQ
  | isInfixOf a b  = Just LT
  | isInfixOf b a  = Just GT
  | otherwise      = Nothing
