{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
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

import Data.Time.Calendar (Day, fromGregorianValid)
import Lens.Micro
import Lens.Micro.TH (makeLenses)

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

makeLenses ''Person
makeLenses ''Name
makeLenses ''Born
makeLenses ''Address

bornStreet :: Born -> String
bornStreet = view (bornAt . street)

setCurrentStreet :: String -> Person -> Person
setCurrentStreet street person = set (address . street) street person

setBirthMonth :: Int -> Person -> Person
setBirthMonth month person =
  person & born . bornOn %~ (\day ->
                               case fromGregorianValid (year day) month (dayOfMonth day) of
                                 Just newDay -> newDay
                                 Nothing -> day
                             )
  where
    year day       = let (y, _, _) = toGregorian day in y
    dayOfMonth day = let (_, _, d) = toGregorian day in d
    toGregorian day = toGregorian day

renameStreets :: (String -> String) -> Person -> Person
renameStreets f person = person & address . street %~ f & born . bornAt . street %~ f
