module Gigasecond (fromDay) where

import Data.Time.Clock (UTCTime(..), addUTCTime, secondsToDiffTime, DiffTime)
import Data.Time.Calendar (Day)

gigasecond :: DiffTime
gigasecond = secondsToDiffTime (10^9)

fromDay :: Day -> UTCTime
fromDay day = addUTCTime gigasecond (UTCTime day 0)
