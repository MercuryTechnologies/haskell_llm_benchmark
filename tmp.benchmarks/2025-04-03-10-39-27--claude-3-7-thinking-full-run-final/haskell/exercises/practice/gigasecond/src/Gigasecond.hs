module Gigasecond (fromDay) where

import Data.Time.Clock (UTCTime, addUTCTime)

fromDay :: UTCTime -> UTCTime
fromDay utcTime = addUTCTime 1000000000 utcTime
