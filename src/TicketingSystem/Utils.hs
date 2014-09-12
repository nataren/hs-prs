{-# LANGUAGE OverloadedStrings #-}
module TicketingSystem.Utils (
  isPullRequestAssociatedToTicket
) where
import qualified Data.Text as T
import qualified YouTrack as YT

getPullRequestTicketNames :: T.Text -> [T.Text]
getPullRequestTicketNames prRef =
  map T.toUpper $ syntacticallyValidTicketNames ticketNames
  where
    ticketNames = T.splitOn "_" prRef
    syntacticallyValidTicketNames =
      filter (\ticketName -> (T.pack "-") `T.isInfixOf` ticketName)

isPullRequestAssociatedToTicket :: T.Text -> IO Bool
isPullRequestAssociatedToTicket prRef = do
  let ticketNames = getPullRequestTicketNames prRef
  ticketingAuth <- YT.mkYouTrackAuth "hostname" "username" "password"
  issues <- sequence $ map (\ticketName -> YT.issueExists ticketingAuth (T.unpack ticketName)) ticketNames
  return $ any id issues

