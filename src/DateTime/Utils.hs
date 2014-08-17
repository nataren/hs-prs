module Datetime.Utils where 

import Data.Time.Format
import Data.Time.Clock
import System.Locale
import qualified Data.Text as T
import Safe

now :: IO UTCTime
now = getCurrentTime

getBranchDate :: T.Text -> Maybe UTCTime
getBranchDate branchDate = parseTime defaultTimeLocale "%Y%m%d" (T.unpack branchDate) :: Maybe UTCTime

targetsOpenBranch :: T.Text -> UTCTime -> Bool
targetsOpenBranch targetBranch rightNow =
  case parsedDate of
    Just date' -> (diffUTCTime date' rightNow) > (fromInteger (138 * 60 * 60) :: NominalDiffTime)
    Nothing -> False
  where
    parsedDate = do
      targetDate <- lastMay $ T.splitOn (T.pack "_") targetBranch
      case getBranchDate targetDate of
        Just targetDate' -> Just targetDate'
        Nothing -> Nothing
  
