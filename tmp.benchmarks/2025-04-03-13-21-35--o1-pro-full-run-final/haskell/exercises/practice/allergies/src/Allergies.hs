module Allergies (Allergen(..), allergies, isAllergicTo) where

import Data.Bits ( (.&.) )

data Allergen
  = Eggs
  | Peanuts
  | Shellfish
  | Strawberries
  | Tomatoes
  | Chocolate
  | Pollen
  | Cats
  deriving (Eq, Show)

valueOfAllergen :: Allergen -> Int
valueOfAllergen Eggs         = 1
valueOfAllergen Peanuts      = 2
valueOfAllergen Shellfish    = 4
valueOfAllergen Strawberries = 8
valueOfAllergen Tomatoes     = 16
valueOfAllergen Chocolate    = 32
valueOfAllergen Pollen       = 64
valueOfAllergen Cats         = 128

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo allergen score =
  (score .&. valueOfAllergen allergen) /= 0

allergies :: Int -> [Allergen]
allergies score =
  filter (`isAllergicTo` score)
  [ Eggs
  , Peanuts
  , Shellfish
  , Strawberries
  , Tomatoes
  , Chocolate
  , Pollen
  , Cats
  ]
