module Github.Domain (PullRequest) where

-- | A Github pull request but classified according to our business rules
data PullRequest = Invalid
                 | AutoMergeable
                 | UnknownMergeability
                 | OpenedNotLinkedToYouTrackIssue
                 | ReopenedNotLinkedToYouTrackIssue
                 | Merged
                 | Skip
