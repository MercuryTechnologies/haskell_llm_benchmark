module Pangram (isPangram) where

import Data.Char (isAlpha, toLower)
import qualified Data.Set as S

isPangram :: String -> Bool
isPangram text = S.size (S.fromList (filter isAlpha (map toLower text))) == 26
