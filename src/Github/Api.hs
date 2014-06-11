import qualified Github.PullRequests as PRS

module Github.Api (get) where
getPullRequests :: [Text] -> IO (Either Error [PullRequest])
get :: undefined

