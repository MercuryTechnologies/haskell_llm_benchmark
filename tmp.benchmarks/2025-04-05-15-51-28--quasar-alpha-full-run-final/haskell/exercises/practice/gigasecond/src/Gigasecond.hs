module Gigasecond (fromDay) where

import Data.Time.Clock (UTCTime, NominalDiffTime, addUTCTime)

fromDay :: UTCTime -> UTCTime
fromDay = addUTCTime gigasecond
  where
    gigasecond :: NominalDiffTime
    gigasecond = 10^(9 :: Integer)
