module PullRequest.Utils where

import Github.Data
import Github.Repos.Webhooks
import Github.Auth
import qualified Data.Text as T
import Data.Functor
import Control.Monad
import qualified Data.Map as M

data MindTouchPullRequestType =
    PullRequestTargetsMasterBranch
  | PullRequestReopenedNotLinkedToYouTrackTicket
  | PullRequestOpenedNotLinkedToYouTrackIssue
  | PullRequestMerged
  | PullRequestUnknownMergeability
  | PullRequestAutoMergeable
  deriving Show

pullRequestTargetsMasterBranch :: DetailedPullRequest -> Bool
pullRequestTargetsMasterBranch pr = (pullRequestCommitRef . detailedPullRequestBase $ pr) == "master"

pullRequestIsMerged :: DetailedPullRequest -> Bool
pullRequestIsMerged = detailedPullRequestMerged

pullRequestMergeabilityIsUnknown :: DetailedPullRequest -> Bool
pullRequestMergeabilityIsUnknown pr = case detailedPullRequestMergeable pr of
  Just _ -> False
  Nothing -> True

pullRequestIsMergeable :: DetailedPullRequest -> Bool
pullRequestIsMergeable pr =
  case detailedPullRequestMergeable pr of
    Just mergeable -> mergeable
    Nothing -> False

pullRequestIsAutoMergeable :: DetailedPullRequest -> Bool
pullRequestIsAutoMergeable pr =
  ((not . detailedPullRequestMerged) $ pr) && (pullRequestIsMergeable pr) && (detailedPullRequestState pr) == "clean"

getPullRequestType :: DetailedPullRequest -> MindTouchPullRequestType
getPullRequestType pr
  | pullRequestTargetsMasterBranch pr = PullRequestTargetsMasterBranch
  | pullRequestIsMerged pr = PullRequestMerged
  | pullRequestMergeabilityIsUnknown pr = PullRequestUnknownMergeability
  | pullRequestIsAutoMergeable pr = PullRequestAutoMergeable

getPullRequestTypeFromEvent :: PullRequestEvent -> Maybe MindTouchPullRequestType
getPullRequestTypeFromEvent ev =
  case pullRequestEventAction ev of
    PullRequestOpened -> Just .getPullRequestType . pullRequestEventPullRequest $ ev
    PullRequestClosed -> Just . getPullRequestType . pullRequestEventPullRequest $ ev
    PullRequestReopened -> Just . getPullRequestType . pullRequestEventPullRequest $ ev
    PullRequestSynchronized -> Nothing
--    PullRequestMerged -> Nothing
--    PullRequestLabeled -> Nothing
--    PullRequestUnlabeled -> Nothing
--    PullRequestAssigned -> Nothing
--    PullRequestUnassigned -> Nothing

-- Webhooks creation
createWebhooks :: GithubAuth -> RepoOwner -> [T.Text] -> T.Text -> [IO (Either Error RepoWebhook)]
createWebhooks auth repoOwner' repos publicUri = do
  mapM (\repo -> createRepoWebhook' auth repoOwner' repo brandNewRepoHook)
    (filterM (\repo -> not . snd <$> webhookExists auth repoOwner' repo publicUri) repos)
    where
      brandNewRepoHook = NewRepoWebhook {
        newRepoWebhookName = "web"
       ,newRepoWebhookConfig = M.fromList [("url", T.unpack publicUri), ("content_type", "json") ]
       ,newRepoWebhookEvents = Just ["pull_request"]
       ,newRepoWebhookActive = Just True
                                        }
  
webhookExists :: GithubAuth -> RepoOwner -> T.Text -> T.Text -> IO (T.Text, Bool)
webhookExists auth repoOwner' repoName' publicUri = do
  possibleWebhooks <- webhooksFor' auth repoOwner' (T.unpack repoName')
  case possibleWebhooks of
    (Left _) -> return (repoName', False)
    (Right webhooks) -> return $ (repoName', (any (\wh -> publicUri == T.pack (repoWebhookUrl wh)) webhooks))
