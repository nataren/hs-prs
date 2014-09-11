{-# LANGUAGE OverloadedStrings #-}
module TicketingSystem.Utils (
  getTicketNames
) where
import qualified Data.Text as T
import qualified YouTrack as YT
getTicketNames :: T.Text -> [T.Text]
getTicketNames prRef =
  map T.toUpper $ syntacticallyValidTicketNames ticketNames
  where
    ticketNames = T.splitOn "_" prRef
    syntacticallyValidTicketNames =
      filter (\ticketName -> (T.pack "-") `T.isInfixOf` ticketName)  
