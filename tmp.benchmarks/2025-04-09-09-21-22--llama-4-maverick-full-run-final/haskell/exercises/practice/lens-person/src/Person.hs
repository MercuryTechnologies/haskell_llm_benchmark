module Person where

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

-- Lenses
lens :: (s -> a) -> (s -> a -> s) -> (a -> f a) -> s -> f s
lens getter setter f s = fmap (setter s) (f (getter s))

name :: Lens' Person Name
name = lens _name (\s a -> s { _name = a })

born :: Lens' Person Born
born = lens _born (\s a -> s { _born = a })

address :: Lens' Person Address
address = lens _address (\s a -> s { _address = a })

foreNames :: Lens' Name String
foreNames = lens _foreNames (\s a -> s { _foreNames = a })

surName :: Lens' Name String
surName = lens _surName (\s a -> s { _surName = a })

bornAt :: Lens' Born Address
bornAt = lens _bornAt (\s a -> s { _bornAt = a })

bornOn :: Lens' Born Day
bornOn = lens _bornOn (\s a -> s { _bornOn = a })

street :: Lens' Address String
street = lens _street (\s a -> s { _street = a })

houseNumber :: Lens' Address Int
houseNumber = lens _houseNumber (\s a -> s { _houseNumber = a })

place :: Lens' Address String
place = lens _place (\s a -> s { _place = a })

country :: Lens' Address String
country = lens _country (\s a -> s { _country = a })

type Lens' s a = (a -> Identity a) -> s -> Identity s

newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure x = Identity x
  (Identity f) <*> (Identity x) = Identity (f x)

-- Functions
bornStreet :: Born -> String
bornStreet = view (bornAt . street)

setCurrentStreet :: String -> Person -> Person
setCurrentStreet streetName = over (address . street) (const streetName)

setBirthMonth :: Int -> Person -> Person
setBirthMonth month person = 
  let (year, _, day) = toGregorian (person ^. born . bornOn)
  in  person & born . bornOn .~ fromGregorian year month day

renameStreets :: (String -> String) -> Person -> Person
renameStreets f = over (address . street) f . over (born . bornAt . street) f

-- Utilities for lenses
view :: Lens' s a -> s -> a
view l = runIdentity . l Identity

over :: Lens' s a -> (a -> a) -> s -> s
over l f = runIdentity . l (Identity . f) 

(&) :: s -> (s -> t) -> t
(&) = flip ($)

(^.) :: s -> Lens' s a -> a
s ^. l = view l s

(.~) :: Lens' s a -> a -> s -> s
l .~ x = over l (const x)
