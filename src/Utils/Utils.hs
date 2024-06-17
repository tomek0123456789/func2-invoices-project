module Utils.Utils (
    getCurrentDate,
    getCurrentDateSuccessor,
)
where

import Data.Time.Calendar
import Data.Time.Clock

getCurrentDate :: IO Day
getCurrentDate = getCurrentTime >>= return . utctDay

getCurrentDateSuccessor :: IO Day
getCurrentDateSuccessor = getCurrentDate >>= return . succ
