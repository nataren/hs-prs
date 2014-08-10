module PullRequest.Utils where

import Github.Data
import Github.Repos.Webhooks
import Github.Auth
import qualified Data.Text as T
import Data.Functor
import Control.Monad
import qualified Data.Map as M

-- | The 'MindTouchPullRequestType' represents the different ways we
-- classify a pull request
data MindTouchPullRequestType =
    PullRequestTargetsMasterBranch
  | PullRequestReopenedNotLinkedToYouTrackTicket
  | PullRequestOpenedNotLinkedToYouTrackIssue
  | PullRequestMerged
  | PullRequestUnknownMergeability
  | PullRequestAutoMergeable
  deriving Show

-- | Determine if a pull request is targetting the master branch
pullRequestTargetsMasterBranch :: DetailedPullRequest -> Bool
pullRequestTargetsMasterBranch pr = (pullRequestCommitRef . detailedPullRequestBase $ pr) == "master"

-- | Determine is a pull request is merged
pullRequestIsMerged :: DetailedPullRequest -> Bool
pullRequestIsMerged = detailedPullRequestMerged

-- | Determine if the mergeability of a given pull request is unknow
-- at this point
pullRequestMergeabilityIsUnknown :: DetailedPullRequest -> Bool
pullRequestMergeabilityIsUnknown pr = case detailedPullRequestMergeable pr of
  Just _ -> False
  Nothing -> True

-- | Determine if a pull request is mergeable
pullRequestIsMergeable :: DetailedPullRequest -> Bool
pullRequestIsMergeable pr =
  case detailedPullRequestMergeable pr of
    Just mergeable -> mergeable
    Nothing -> False

-- | Determine if the pull request is considered auto-mergeable by the bot
pullRequestIsAutoMergeable :: DetailedPullRequest -> Bool
pullRequestIsAutoMergeable pr =
  ((not . detailedPullRequestMerged) $ pr) && (pullRequestIsMergeable pr) && (detailedPullRequestState pr) == "clean"

getPullRequestType :: DetailedPullRequest -> MindTouchPullRequestType
getPullRequestType pr
  | pullRequestTargetsMasterBranch pr = PullRequestTargetsMasterBranch
  | pullRequestIsMerged pr = PullRequestMerged
  | pullRequestMergeabilityIsUnknown pr = PullRequestUnknownMergeability
  | pullRequestIsAutoMergeable pr = PullRequestAutoMergeable

-- | Given a 'PullRequestEvent' find out which pull request type is
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

-- | How to 'createWebhooks'
createWebhooks :: GithubAuth -> RepoOwner -> [T.Text] -> T.Text -> IO [(Either Error RepoWebhook)]
createWebhooks auth repoOwner' repos publicUri = do
  reposToCreate <- (filterM (\repo -> not . snd <$> webhookExists auth repoOwner' repo publicUri) repos)
  mapM (\repo -> createRepoWebhook' auth repoOwner' (T.unpack repo) brandNewRepoHook) reposToCreate
    where
      brandNewRepoHook = NewRepoWebhook {
        newRepoWebhookName = "web"
       ,newRepoWebhookConfig = M.fromList [("url", T.unpack publicUri), ("content_type", "json") ]
       ,newRepoWebhookEvents = Just ["pull_request"]
       ,newRepoWebhookActive = Just True
     }

-- | Check is a webhook with a given URI exists on a repo  
webhookExists :: GithubAuth -> RepoOwner -> T.Text -> T.Text -> IO (T.Text, Bool)
webhookExists auth repoOwner' repoName' publicUri = do
  possibleWebhooks <- webhooksFor' auth repoOwner' (T.unpack repoName')
  case possibleWebhooks of
    (Left _) -> return (repoName', False)
    (Right webhooks) -> return $ (repoName', (any (\wh -> publicUri == T.pack (repoWebhookUrl wh)) webhooks))
