module Gigasecond (fromDay) where

import Data.Time.Clock (UTCTime, NominalDiffTime, addUTCTime)

gigasecond :: NominalDiffTime
gigasecond = 1000000000

fromDay :: UTCTime -> UTCTime
fromDay t = addUTCTime gigasecond t
