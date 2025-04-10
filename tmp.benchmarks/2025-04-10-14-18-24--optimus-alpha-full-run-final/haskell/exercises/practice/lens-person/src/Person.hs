module Person
  ( Address (..)
  , Born    (..)
  , Name    (..)
  , Person  (..)
  , bornStreet
  , renameStreets
  , setBirthMonth
  , setCurrentStreet
  ) where

import Data.Time.Calendar (Day, toGregorian, fromGregorian)

data Person = Person { _name    :: Name
                     , _born    :: Born
                     , _address :: Address
                     }

data Name = Name { _foreNames :: String
                 , _surName   :: String
                 }

data Born = Born { _bornAt :: Address
                 , _bornOn :: Day
                 }

data Address = Address { _street      :: String
                       , _houseNumber :: Int
                       , _place       :: String
                       , _country     :: String
                       }

-- Lenses (manually written)
name :: (Name -> Name) -> Person -> Person
name f p = p { _name = f (_name p) }

born :: (Born -> Born) -> Person -> Person
born f p = p { _born = f (_born p) }

address :: (Address -> Address) -> Person -> Person
address f p = p { _address = f (_address p) }

foreNames :: (String -> String) -> Name -> Name
foreNames f n = n { _foreNames = f (_foreNames n) }

surName :: (String -> String) -> Name -> Name
surName f n = n { _surName = f (_surName n) }

bornAt :: (Address -> Address) -> Born -> Born
bornAt f b = b { _bornAt = f (_bornAt b) }

bornOn :: (Day -> Day) -> Born -> Born
bornOn f b = b { _bornOn = f (_bornOn b) }

street :: (String -> String) -> Address -> Address
street f a = a { _street = f (_street a) }

houseNumber :: (Int -> Int) -> Address -> Address
houseNumber f a = a { _houseNumber = f (_houseNumber a) }

place :: (String -> String) -> Address -> Address
place f a = a { _place = f (_place a) }

country :: (String -> String) -> Address -> Address
country f a = a { _country = f (_country a) }

-- Functions to implement

bornStreet :: Born -> String
bornStreet b = _street (_bornAt b)

setCurrentStreet :: String -> Person -> Person
setCurrentStreet s = address (street (const s))

setBirthMonth :: Int -> Person -> Person
setBirthMonth m = born (bornOn (\d -> let (y,_,d') = toGregorian d in fromGregorian y m d'))

renameStreets :: (String -> String) -> Person -> Person
renameStreets f =
  address (street f) .
  born (bornAt (street f))
