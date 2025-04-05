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

allergenList :: [(Allergen, Int)]
allergenList = [ (Eggs, 1)
               , (Peanuts, 2)
               , (Shellfish, 4)
               , (Strawberries, 8)
               , (Tomatoes, 16)
               , (Chocolate, 32)
               , (Pollen, 64)
               , (Cats, 128)
               ]

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo allergen score =
    case lookup allergen allergenList of
        Just val -> (score .&. val) /= 0
        Nothing  -> False

allergies :: Int -> [Allergen]
allergies score = [ allergen | (allergen, val) <- allergenList, (score .&. val) /= 0 ]
