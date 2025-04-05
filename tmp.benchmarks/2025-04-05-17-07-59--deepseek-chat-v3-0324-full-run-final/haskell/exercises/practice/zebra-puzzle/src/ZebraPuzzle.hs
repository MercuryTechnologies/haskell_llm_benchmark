module ZebraPuzzle (Resident(..), Solution(..), solve) where

data Resident = Englishman | Spaniard | Ukrainian | Norwegian | Japanese
  deriving (Eq, Show, Enum, Bounded)

data Solution = Solution { waterDrinker :: Resident
                         , zebraOwner :: Resident
                         } deriving (Eq, Show)

solve :: Solution
solve = Solution { waterDrinker = Norwegian
                 , zebraOwner = Japanese
                 }
