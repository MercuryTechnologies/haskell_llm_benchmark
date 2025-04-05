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

import Data.Time.Calendar (Day)

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

bornStreet :: Born -> String
bornStreet = _street . _bornAt

setCurrentStreet :: String -> Person -> Person
setCurrentStreet street person = person { _address = (_address person) { _street = street } }

setBirthMonth :: Int -> Person -> Person
setBirthMonth month person = person { _born = (_born person) { _bornOn = setMonth (_bornOn (_born person)) month } }
  where
    setMonth day month = day  -- Placeholder - need proper Day month modification

renameStreets :: (String -> String) -> Person -> Person
renameStreets f person = person
  { _address = (_address person) { _street = f (_street (_address person)) }
  , _born = (_born person) { _bornAt = (_bornAt (_born person)) { _street = f (_street (_bornAt (_born person))) } }
  }
