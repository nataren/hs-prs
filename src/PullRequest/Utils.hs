module PullRequest.Utils where

import Github.Data

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
  Just mergeable -> False
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
