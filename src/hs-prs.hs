{-# LANGUAGE OverloadedStrings #-}

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
  githubToken <- getEnv "GITHUB_TOKEN"
  githubOwner <- getEnv "GITHUB_OWNER"
  githubRepos <- getEnv "GITHUB_REPOS"
  publicUri <- getEnv "PRS_PUBLIC_URI"
  -- mergeRetries <- getEnvironmentVariable "MERGE_RETRIES"
  -- mergeTTL <- getEnvironmentVariable "MERGE_TTL"
  -- mergeabilityRetries <- getEnvironmentVariable "MERGEABILITY_RETRIES"
  -- mergeabilityTTL <- getEnvironmentVariable "MERGEABILITY_TTL"
  -- github2youtrack <- getEnvironmentVariable "GITHUB_2_YOUTRACK"
  -- youtrackHostname <- getEnvironmentVariable "YOUTRACK_HOSTNAME"
  -- youtrackUsername <- getEnvironmentVariable "YOUTRACK_USERNAME"
  -- youtrackPassword <- getEnvironmentVariable "YOUTRACK_PASSWORD"
  -- archiveBranchesTTL <- getEnvironmentVariable "ARCHIVE_BRANCHES_TTL"
  -- archiveBranchesToKeep <- getEnvironmentVariable "ARCHIVE_BRANCHES_TO_KEEP"
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
