module Gigasecond (fromDay) where

import Data.Time.Clock (UTCTime, addUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)

fromDay :: UTCTime -> UTCTime
fromDay time = addUTCTime gigasecond time
  where gigasecond = 10^9
