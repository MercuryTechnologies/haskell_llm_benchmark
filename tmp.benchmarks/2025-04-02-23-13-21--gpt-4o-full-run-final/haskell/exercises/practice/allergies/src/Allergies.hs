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

allergenValue :: Allergen -> Int
allergenValue Eggs = 1
allergenValue Peanuts = 2
allergenValue Shellfish = 4
allergenValue Strawberries = 8
allergenValue Tomatoes = 16
allergenValue Chocolate = 32
allergenValue Pollen = 64
allergenValue Cats = 128

allergies :: Int -> [Allergen]
allergies score = filter (\allergen -> isAllergicTo allergen score) allAllergens
  where
    allAllergens = [Eggs, Peanuts, Shellfish, Strawberries, Tomatoes, Chocolate, Pollen, Cats]

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo allergen score = (score .&. allergenValue allergen) /= 0
