{-# LANGUAGE OverloadedStrings #-}
module TicketingSystem.Utils (
  isPullRequestAssociatedToTicket
) where
import Control.Monad.Trans.Reader
import qualified Data.Text as T
import qualified YouTrack as YT
import Control.Monad.IO.Class

getPullRequestTicketNames :: T.Text -> [T.Text]
getPullRequestTicketNames prRef =
  map T.toUpper $ syntacticallyValidTicketNames ticketNames
  where
    ticketNames = T.splitOn "_" prRef
    syntacticallyValidTicketNames =
      filter (\ticketName -> (T.pack "-") `T.isInfixOf` ticketName)

isPullRequestAssociatedToTicket :: T.Text -> ReaderT YT.YouTrackAuth IO Bool
isPullRequestAssociatedToTicket prRef = do
  let ticketNames = getPullRequestTicketNames prRef
  ticketingAuth <- ask
  liftIO $ do
    issues <- sequence $ map (\ticketName -> runReaderT (YT.issueExists (T.unpack ticketName))  ticketingAuth) ticketNames
    return $ any id issues

