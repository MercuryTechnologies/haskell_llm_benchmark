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
              deriving (Eq, Show, Enum, Bounded)

allergens :: [Allergen]
allergens = [minBound..maxBound]

allergyValue :: Allergen -> Int
allergyValue = (2^) . fromEnum

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo allergen score = (score .&. allergyValue allergen) /= 0

allergies :: Int -> [Allergen]
allergies score = filter (\a -> isAllergicTo a score) allergens
