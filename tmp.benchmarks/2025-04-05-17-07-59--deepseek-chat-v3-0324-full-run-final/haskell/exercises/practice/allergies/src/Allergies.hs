module Allergies (Allergen(..), allergies, isAllergicTo) where

import Data.Bits ((.&.))

data Allergen = Eggs
              | Peanuts
              | Shellfish
              | Strawberries
              | Tomatoes
              | Chocolate
              | Pollen
              | Cats
              deriving (Eq, Show)

allergyValues :: [(Allergen, Int)]
allergyValues = [ (Eggs, 1)
                , (Peanuts, 2)
                , (Shellfish, 4)
                , (Strawberries, 8)
                , (Tomatoes, 16)
                , (Chocolate, 32)
                , (Pollen, 64)
                , (Cats, 128)
                ]

allergies :: Int -> [Allergen]
allergies score = [ allergen | (allergen, value) <- allergyValues, score .&. value == value ]

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo allergen score = any (\(a, v) -> a == allergen && score .&. v == v) allergyValues
