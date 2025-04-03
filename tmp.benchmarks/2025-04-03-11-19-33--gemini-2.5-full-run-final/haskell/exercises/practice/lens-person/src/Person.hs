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
  -- Export lenses for testing or further use if needed
  , name
  , born
  , address
  , foreNames
  , surName
  , bornAt
  , bornOn
  , street
  , houseNumber
  , place
  , country
  ) where

import Control.Lens
import Data.Time.Calendar (Day, toGregorian, fromGregorian)
-- Removed import Data.Time.Calendar.Types

data Person = Person { _name    :: Name
                     , _born    :: Born
                     , _address :: Address
                     } deriving (Show, Eq)

data Name = Name { _foreNames :: String
                 , _surName   :: String
                 } deriving (Show, Eq)

data Born = Born { _bornAt :: Address
                 , _bornOn :: Day
                 } deriving (Show, Eq)

data Address = Address { _street      :: String
                       , _houseNumber :: Int
                       , _place       :: String
                       , _country     :: String
                       } deriving (Show, Eq)

-- Generate lenses for each record field
makeLenses ''Person
makeLenses ''Name
makeLenses ''Born
makeLenses ''Address

-- | Get the street name of the place where the person was born.
bornStreet :: Person -> String
bornStreet p = p ^. born . bornAt . street
-- Alternative implementation using view:
-- bornStreet = view (born . bornAt . street)


-- | Set the street name of the person's current address.
setCurrentStreet :: String -> Person -> Person
setCurrentStreet newStreet p = p & address . street .~ newStreet
-- Alternative implementation using set:
-- setCurrentStreet newStreet = set (address . street) newStreet

-- | Set the month of the person's birth date.
setBirthMonth :: Int -> Person -> Person
setBirthMonth newMonth p = p & born . bornOn %~ setDayMonth newMonth
  where
    -- Helper function to update the month of a Day
    setDayMonth :: Int -> Day -> Day
    setDayMonth m d = let (y, _, dy) = toGregorian d
                      in fromGregorian y m dy

-- | Apply a function to rename streets in both bornAt and current address.
renameStreets :: (String -> String) -> Person -> Person
renameStreets f p = p & (address . street %~ f) . (born . bornAt . street %~ f)
-- Alternative implementation composing 'over':
-- renameStreets f = over (address . street) f . over (born . bornAt . street) f
