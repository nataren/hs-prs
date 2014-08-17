module Datetime.Utils where 

import Date.Time.Format
import Date.Time.Clock
import Date.Time.LocalTime
import System.Locale

now :: IO UTCTime
now :: getCurrentTime

getBranchDate :: String -> Maybe UTCTime
getBranchDate branchName = parseTime defaultTimeLocale "%Y%m%d" branchName :: Maybe UTCTime
