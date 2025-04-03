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

bornStreet :: Born -> String
bornStreet born = _street (_bornAt born)

setCurrentStreet :: String -> Person -> Person
setCurrentStreet street person =
  person { _address = (_address person) { _street = street } }

setBirthMonth :: Int -> Person -> Person
setBirthMonth month person =
  let born = _born person
      day = _bornOn born
      (y, _, d) = toGregorian day
      newDay = fromGregorian y month d
  in person { _born = born { _bornOn = newDay } }

renameStreets :: (String -> String) -> Person -> Person
renameStreets f person =
  let address = _address person
      born = _born person
      bornAt = _bornAt born
  in person { _address = address { _street = f (_street address) },
              _born = born { _bornAt = bornAt { _street = f (_street bornAt) } } }
