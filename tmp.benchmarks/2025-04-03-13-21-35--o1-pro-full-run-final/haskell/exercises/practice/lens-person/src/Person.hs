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

data Person = Person
  { _name    :: Name
  , _born    :: Born
  , _address :: Address
  }

data Name = Name
  { _foreNames :: String
  , _surName   :: String
  }

data Born = Born
  { _bornAt :: Address
  , _bornOn :: Day
  }

data Address = Address
  { _street      :: String
  , _houseNumber :: Int
  , _place       :: String
  , _country     :: String
  }

-- | Return the street where the person was born
bornStreet :: Born -> String
bornStreet born =
  _street (_bornAt born)

-- | Set the current street of the person
setCurrentStreet :: String -> Person -> Person
setCurrentStreet street person =
  person { _address = (_address person) { _street = street } }

-- | Change only the birth month, leaving the day and year intact
setBirthMonth :: Int -> Person -> Person
setBirthMonth month person =
  let (year, _, dayOfMonth) = toGregorian (_bornOn (_born person))
   in person
        { _born = (_born person)
            { _bornOn = fromGregorian year month dayOfMonth
            }
        }

-- | Apply a function to rename all streets in this Person
renameStreets :: (String -> String) -> Person -> Person
renameStreets f person =
  let oldBorn    = _born person
      oldBornAt  = _bornAt oldBorn
      oldAddress = _address person
   in person
        { _born =
            oldBorn
              { _bornAt = oldBornAt { _street = f (_street oldBornAt) }
              }
        , _address = oldAddress { _street = f (_street oldAddress) }
        }
