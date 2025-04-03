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

-- Map each allergen to its score value
allergenScore :: Allergen -> Int
allergenScore Eggs         = 1
allergenScore Peanuts      = 2
allergenScore Shellfish    = 4
allergenScore Strawberries = 8
allergenScore Tomatoes     = 16
allergenScore Chocolate    = 32
allergenScore Pollen       = 64
allergenScore Cats         = 128

-- List of all allergens
allAllergens :: [Allergen]
allAllergens = [Eggs, Peanuts, Shellfish, Strawberries, Tomatoes, Chocolate, Pollen, Cats]

-- Return the list of allergens a person has based on their score
allergies :: Int -> [Allergen]
allergies score = filter (\allergen -> isAllergicTo allergen score) allAllergens

-- Check if a person is allergic to a specific allergen based on their score
isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo allergen score = (score .&. allergenScore allergen) /= 0
