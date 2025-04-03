{-# LANGUAGE RankNTypes #-}
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
import Data.Functor.Identity

-- Minimal lens definitions using van Laarhoven lenses
type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

over :: Lens' s a -> (a -> a) -> s -> s
over lens f s = runIdentity (lens (Identity . f) s)

set :: Lens' s a -> a -> s -> s
set lens value s = over lens (const value) s

-- Lens for Address _street field
streetL :: Lens' Address String
streetL f addr = fmap (\s -> addr { _street = s }) (f (_street addr))

-- Lens for Person _address field
addressL :: Lens' Person Address
addressL f p = fmap (\addr -> p { _address = addr }) (f (_address p))

-- Lens for Born _bornAt field
bornAtL :: Lens' Born Address
bornAtL f b = fmap (\a -> b { _bornAt = a }) (f (_bornAt b))

-- Lens for Person _born field
bornL :: Lens' Person Born
bornL f p = fmap (\born -> p { _born = born }) (f (_born p))

-- Implementations using lenses

bornStreet :: Born -> String
bornStreet b = _street (_bornAt b)

setCurrentStreet :: String -> Person -> Person
setCurrentStreet newStreet person = set (addressL . streetL) newStreet person

setBirthMonth :: Int -> Person -> Person
setBirthMonth newMonth person =
  let b       = _born person
      (year, _, day) = toGregorian (_bornOn b)
      newBornOn = fromGregorian year newMonth day
      newBorn   = b { _bornOn = newBornOn }
  in person { _born = newBorn }

renameStreets :: (String -> String) -> Person -> Person
renameStreets f person =
  let personUpdated  = over (addressL . streetL) f person
      personFinal    = over (bornL . bornAtL . streetL) f personUpdated
  in personFinal

-- Data declarations

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
