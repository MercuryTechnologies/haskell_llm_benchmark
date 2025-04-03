module Gigasecond (fromDay) where

import Data.Time.Clock (UTCTime, NominalDiffTime, addUTCTime)

gigasecond :: NominalDiffTime
gigasecond = fromIntegral (10^9 :: Integer)

fromDay :: UTCTime -> UTCTime
fromDay = addUTCTime gigasecond
