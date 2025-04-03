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
  ) where

import Data.Time.Calendar (Day, fromGregorian, toGregorian)
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
bornStreet born = born ^. bornAt . street

setCurrentStreet :: String -> Person -> Person
setCurrentStreet street person = person & address . street .~ street

setBirthMonth :: Int -> Person -> Person
setBirthMonth month person = person & born . bornOn %~ setMonth
  where
    setMonth day = let (year, _, dayOfMonth) = toGregorian day
                   in fromGregorian year month dayOfMonth

renameStreets :: (String -> String) -> Person -> Person
renameStreets f person = person & address . street %~ f
                                & born . bornAt . street %~ f
