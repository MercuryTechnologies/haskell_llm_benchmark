{-# LANGUAGE TemplateHaskell #-}

module Person
  ( Address (..)
  , Born    (..)
  , Name    (..)
  , Person  (..)
  , bornStreet
  , renameStreets
  , setBirthMonth
  , setCurrentStreet
  , _name
  , _born
  , _address
  , _foreNames
  , _surName
  , _bornAt
  , _bornOn
  , _street
  , _houseNumber
  , _place
  , _country
  ) where

import Data.Time.Calendar (Day)
import Control.Lens

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

makeLenses ''Person
makeLenses ''Name
makeLenses ''Born
makeLenses ''Address

bornStreet :: Born -> String
bornStreet born = view (bornAt . street) born

setCurrentStreet :: String -> Person -> Person
setCurrentStreet street person = person & address . street .~ street

setBirthMonth :: Int -> Person -> Person
setBirthMonth month person = person & born . bornOn .~ (fromGregorian (year (person ^. born . bornOn)) month (day (person ^. born . bornOn)))

renameStreets :: (String -> String) -> Person -> Person
renameStreets f person = person & address . street %~ f
