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

-- Lenses for Person
nameLens :: (Name -> Name) -> Person -> Person
nameLens f p = p { _name = f (_name p) }

bornLens :: (Born -> Born) -> Person -> Person
bornLens f p = p { _born = f (_born p) }

addressLens :: (Address -> Address) -> Person -> Person
addressLens f p = p { _address = f (_address p) }

-- Lenses for Name
foreNamesLens :: (String -> String) -> Name -> Name
foreNamesLens f n = n { _foreNames = f (_foreNames n) }

surNameLens :: (String -> String) -> Name -> Name
surNameLens f n = n { _surName = f (_surName n) }

-- Lenses for Born
bornAtLens :: (Address -> Address) -> Born -> Born
bornAtLens f b = b { _bornAt = f (_bornAt b) }

bornOnLens :: (Day -> Day) -> Born -> Born
bornOnLens f b = b { _bornOn = f (_bornOn b) }

-- Lenses for Address
streetLens :: (String -> String) -> Address -> Address
streetLens f a = a { _street = f (_street a) }

houseNumberLens :: (Int -> Int) -> Address -> Address
houseNumberLens f a = a { _houseNumber = f (_houseNumber a) }

placeLens :: (String -> String) -> Address -> Address
placeLens f a = a { _place = f (_place a) }

countryLens :: (String -> String) -> Address -> Address
countryLens f a = a { _country = f (_country a) }

bornStreet :: Born -> String
bornStreet born = _street (_bornAt born)

setCurrentStreet :: String -> Person -> Person
setCurrentStreet street =
  addressLens (streetLens (const street))

setBirthMonth :: Int -> Person -> Person
setBirthMonth month =
  bornLens (bornOnLens updateMonth)
  where
    updateMonth day =
      let (y, _, d) = toGregorian day
      in fromGregorian y (fromIntegral month) d

renameStreets :: (String -> String) -> Person -> Person
renameStreets f =
  addressLens (streetLens f) .
  bornLens (bornAtLens (streetLens f))
