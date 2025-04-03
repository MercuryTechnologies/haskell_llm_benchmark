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

import Data.Time.Calendar (Day, fromGregorian, toGregorian)

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

-- Manual lens implementations
-- Getter for Person's name
name :: Person -> Name
name = _name

-- Setter for Person's name
setName :: Name -> Person -> Person
setName n p = p { _name = n }

-- Getter for Person's born
born :: Person -> Born
born = _born

-- Setter for Person's born
setBorn :: Born -> Person -> Person
setBorn b p = p { _born = b }

-- Getter for Person's address
address :: Person -> Address
address = _address

-- Setter for Person's address
setAddress :: Address -> Person -> Person
setAddress a p = p { _address = a }

-- Getter for Born's bornAt
bornAt :: Born -> Address
bornAt = _bornAt

-- Setter for Born's bornAt
setBornAt :: Address -> Born -> Born
setBornAt a b = b { _bornAt = a }

-- Getter for Born's bornOn
bornOn :: Born -> Day
bornOn = _bornOn

-- Setter for Born's bornOn
setBornOn :: Day -> Born -> Born
setBornOn d b = b { _bornOn = d }

-- Getter for Address's street
street :: Address -> String
street = _street

-- Setter for Address's street
setStreet :: String -> Address -> Address
setStreet s a = a { _street = s }

-- Get the street name where a person was born
bornStreet :: Born -> String
bornStreet born' = street (bornAt born')

-- Set the current street of a person
setCurrentStreet :: String -> Person -> Person
setCurrentStreet s person = 
  setAddress (setStreet s (address person)) person

-- Set the birth month of a person
setBirthMonth :: Int -> Person -> Person
setBirthMonth month person =
  setBorn (setBornOn (updateMonth (bornOn (born person))) (born person)) person
  where
    updateMonth day =
      let (year, _, day') = toGregorian day
      in fromGregorian year month day'

-- Rename all streets in a person record using the provided function
renameStreets :: (String -> String) -> Person -> Person
renameStreets f person =
  let 
    -- Update current address street
    newAddress = setStreet (f (street (address person))) (address person)
    -- Update birth address street
    newBornAt = setStreet (f (street (bornAt (born person)))) (bornAt (born person))
    newBorn = setBornAt newBornAt (born person)
  in
    setAddress newAddress (setBorn newBorn person)
