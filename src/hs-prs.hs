{-# LANGUAGE OverloadedStrings #-}
-- | The entry point to the 'PullRequestService' web service
module PullRequestService where

import Network.Wai.Middleware.RequestLogger
import qualified Web.Scotty as S
import qualified Data.Text as T
import Data.Aeson
import Github.Data
import Github.Auth
import PullRequest.Utils
import Control.Monad
import System.Environment

main :: IO ()
main = do
  -- Read all the web services configuration values
  githubToken <- getEnv "PRS_GITHUB_TOKEN"
  githubOwner <- getEnv "PRS_GITHUB_OWNER"
  githubRepos <- getEnv "PRS_GITHUB_REPOS"
  publicUri <- getEnv "PRS_PUBLIC_URI"
  mergeRetries <- getEnv "PRS_MERGE_RETRIES"
  mergeTTL <- getEnv "PRS_MERGE_TTL"
  mergeabilityRetries <- getEnv "PRS_MERGEABILITY_RETRIES"
  mergeabilityTTL <- getEnv "PRS_MERGEABILITY_TTL"
  github2youtrack <- getEnv "PRS_GITHUB_2_YOUTRACK"
  youtrackHostname <- getEnv "PRS_YOUTRACK_HOSTNAME"
  youtrackUsername <- getEnv "PRS_YOUTRACK_USERNAME"
  youtrackPassword <- getEnv "PRS_YOUTRACK_PASSWORD"
  archiveBranchesTTL <- getEnv "PRS_ARCHIVE_BRANCHES_TTL"
  archiveBranchesToKeep <- getEnv "PRS_ARCHIVE_BRANCHES_TO_KEEP"
  let repos = T.splitOn "," (T.pack githubRepos)
  let auth = GithubOAuth githubToken
  _ <- createWebhooks auth githubOwner repos (T.pack publicUri)
  S.scotty 3000 $ do
    S.middleware logStdoutDev
  
    S.get "/status" $ do
      S.json . T.pack $ "Working fine ..."

    S.post "/pr/notify" $ do
      b <- S.body
      let event = decode b :: Maybe PullRequestEvent
      case event of
        Just ev -> S.json $ show . getPullRequestTypeFromEvent $ ev
        Nothing -> S.json . T.pack $ ""
