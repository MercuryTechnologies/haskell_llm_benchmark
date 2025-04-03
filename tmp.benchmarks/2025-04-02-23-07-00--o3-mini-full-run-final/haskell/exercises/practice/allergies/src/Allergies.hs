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

allergenScore :: Allergen -> Int
allergenScore Eggs         = 1
allergenScore Peanuts      = 2
allergenScore Shellfish    = 4
allergenScore Strawberries = 8
allergenScore Tomatoes     = 16
allergenScore Chocolate    = 32
allergenScore Pollen       = 64
allergenScore Cats         = 128

allergensList :: [Allergen]
allergensList = [Eggs, Peanuts, Shellfish, Strawberries, Tomatoes, Chocolate, Pollen, Cats]

allergies :: Int -> [Allergen]
allergies score = filter (\a -> isAllergicTo a score) allergensList

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo allergen score = (score .&. allergenScore allergen) /= 0
