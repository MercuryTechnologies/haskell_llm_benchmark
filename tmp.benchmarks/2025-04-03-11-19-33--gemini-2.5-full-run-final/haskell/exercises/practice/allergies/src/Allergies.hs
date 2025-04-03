module Allergies (Allergen(..), allergies, isAllergicTo) where

import Data.Bits ((.&.))

-- | The list of substances that can cause an allergic reaction.
-- Added Enum and Bounded for easier iteration over all allergens.
data Allergen = Eggs
              | Peanuts
              | Shellfish
              | Strawberries
              | Tomatoes
              | Chocolate
              | Pollen
              | Cats
              deriving (Eq, Show, Enum, Bounded)

-- | A helper function to get the score associated with a specific allergen.
allergenScore :: Allergen -> Int
allergenScore Eggs         = 1
allergenScore Peanuts      = 2
allergenScore Shellfish    = 4
allergenScore Strawberries = 8
allergenScore Tomatoes     = 16
allergenScore Chocolate    = 32
allergenScore Pollen       = 64
allergenScore Cats         = 128

-- | A list of all known allergens. Uses the Enum and Bounded instances.
allAllergens :: [Allergen]
allAllergens = [minBound .. maxBound]

-- | Given a person's allergy score, determine their full list of allergies.
-- Allergens are determined by checking if the corresponding bit is set in the score.
-- Scores for allergens not listed (>= 256) are ignored.
allergies :: Int -> [Allergen]
allergies score = filter (\allergen -> isAllergicTo allergen score) allAllergens

-- | Given a person's allergy score, determine whether or not they're allergic to a given item.
-- Uses bitwise AND to check if the allergen's specific bit is set in the score.
isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo allergen score = (score .&. allergenScore allergen) /= 0
