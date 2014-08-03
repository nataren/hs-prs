{-# LANGUAGE OverloadedStrings #-}

import Github.Data

module PullRequest.Utils where

data MindTouchPullRequestType =
    TargetsMasterBranch
  | ReopenedNotLinkedToYouTrackTicket
  | OpenedNotLinkedToYouTrackIssue
  | Merged
  | UnknownMergeability
  | AutoMergeable
  deriving Show

pullRequestTargetsMasterBranch :: DetailedPullRequest -> Bool
pullRequestTargetsMasterBranch pr = (pullRequestCommitRef . detailedPullRequestBase $ pr) == "master"

pullRequestIsMerged :: DetailedPullRequest -> Bool
pullRequestIsMerged = detailedPullRequestMerged

pullRequestMergeabilityIsUnknown :: DetailedPullRequest -> Bool
pullRequestMergeabilityIsUnknown pr = case detailedPullRequestMergeable pr of
  Just mergeable -> false
  Nothing -> true

pullRequestIsMergeable :: DetailedPullRequest -> Bool
pullRequestIsMergeable pr =
  case detailedPullRequestMergeable pr of
    Just mergeable -> mergeable
    Nothing -> false

pullRequestIsAutoMergeable :: DetailedPullRequest -> Bool
pullRequestIsAutoMergeable pr =
  not . detailedPullRequestMerged $ pr & pullRequestIsMergeable pr & detailedPullRequestState pr == "clean"

getPullRequestType :: DetailedPullRequest -> MindTouchPullRequestType
getPullRequestType pr =
  pullRequestTargetsMasterBranch pr = TargetsMasterBranch
  pullRequestIsMerged pr = Merged
  pullRequestMergeabilityIsUnknown pr = UnknownMergeability
  pullRequestIsAutoMergeable pr = AutoMergeable

getPullRequestTypeFromEvent :: PullRequestEvent -> Maybe MindTouchPullRequestType
getPullRequestTypeFromEvent ev =
  case pullRequestEventAction ev of
    PullRequestOpened -> undefined
    PullRequestClosed -> undefined
    PullRequestReopened -> undefined
    PullRequestSynchronized | PullRequestAssigned | PullRequestUnassigned
    | PullRequestLabeled | PullRequestUnlabeled -> Nothing
