module Datetime.Utils where 

import Data.Time.Format
import Data.Time.Clock
import Data.Time.LocalTime
import System.Locale
import qualified Data.Text as T
import Safe

now :: IO UTCTime
now = getCurrentTime

getBranchDate :: T.Text -> Maybe UTCTime
getBranchDate branchDate = parseTime defaultTimeLocale "%Y%m%d" (T.unpack branchDate) :: Maybe UTCTime

targetsOpenBranch :: T.Text -> UTCTime -> IO Bool
targetsOpenBranch branchName rightNow =
  case parsedDate of
    Just date' -> return $ (diffUTCTime date' rightNow) > secondsToDiffTime (138 * 60 * 60)
    Nothing -> return $ False
  where
    parsedDate = lastMay $ T.splitOn (T.pack "_") branchName >>= \d -> getBranchDate d

  
