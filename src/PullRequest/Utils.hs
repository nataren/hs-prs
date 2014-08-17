module PullRequest.Utils where

import Github.Data
import Github.Repos.Webhooks
import Github.Auth
import Github.PullRequests
import Github.Issues.Comments
import Github.Issues.Events
import Data.Functor
import Control.Monad
import Data.Either
import Safe
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Function as F

-- | The 'MindTouchPullRequestType' represents the different ways we
-- classify a pull request
data MindTouchPullRequestType =
    PullRequestIsUncategorized DetailedPullRequest
  | PullRequestTargetsMasterBranch DetailedPullRequest
  | PullRequestReopenedNotLinkedToYouTrackTicket DetailedPullRequest
  | PullRequestOpenedNotLinkedToYouTrackIssue DetailedPullRequest
  | PullRequestMerged DetailedPullRequest
  | PullRequestUnknownMergeability DetailedPullRequest
  | PullRequestAutoMergeable DetailedPullRequest
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

pullRequestIsOpen :: DetailedPullRequest -> Bool
pullRequestIsOpen dpr = state == "open" || state == "reopen"
  where state = detailedPullRequestState dpr

pullRequestIsReopened :: DetailedPullRequest -> IO Bool
pullRequestIsReopened dpr = do
  events <- eventsForIssue owner' repo' issueNumber'
  case events of
    Left err -> return False
    Right events' -> do
      let lastEvent = lastMay events'
      case lastEvent of
        Just lastEvent' -> return $ eventType lastEvent' == Reopened
        Nothing -> return False
  where
    repo' = getRepoName dpr
    owner' = getOwner dpr
    issueNumber' = getIssueNumber dpr

getPullRequestType :: DetailedPullRequest -> IO MindTouchPullRequestType
getPullRequestType dpr = do
  prTargetsMasterBranch <- return $ pullRequestTargetsMasterBranch dpr
  if prTargetsMasterBranch
    then return $ PullRequestTargetsMasterBranch dpr
    else do
    isReopened <- pullRequestIsReopened dpr
    if isReopened && (pullRequestIsOpen dpr)
      then return $ PullRequestReopenedNotLinkedToYouTrackTicket dpr
      else return $ PullRequestIsUncategorized dpr

-- | Given a 'PullRequestEvent' find out which pull request type is
getPullRequestTypeFromEvent :: PullRequestEvent -> IO MindTouchPullRequestType
getPullRequestTypeFromEvent ev = do
  eventType' <- return $ pullRequestEventAction ev 
  type' <- getPullRequestType . pullRequestEventPullRequest $ ev
  case eventType' of
    PullRequestOpened -> return type'
    PullRequestClosed -> return type'
    PullRequestReopened -> return type'

processPullRequestType :: GithubAuth -> MindTouchPullRequestType -> IO (Either Error DetailedPullRequest)
processPullRequestType auth prType = case prType of
  PullRequestTargetsMasterBranch dpr -> do
    _ <- commentOnPullRequest auth dpr (T.pack "This pull request is invalid because it targets the master branch")
    closeRes <- closePullRequest auth dpr
    return closeRes
  PullRequestReopenedNotLinkedToYouTrackTicket dpr -> do
    _ <- commentOnPullRequest auth dpr (T.pack "This *reopened* pull request is not bound to a YouTrack issue, it will be ignored but human intervention is required")
    return $ Right dpr
  PullRequestOpenedNotLinkedToYouTrackIssue dpr -> undefined
  PullRequestMerged dpr -> undefined
  PullRequestUnknownMergeability dpr -> undefined
  PullRequestAutoMergeable dpr -> undefined

-- | How to 'createWebhooks'
createWebhooks :: GithubAuth -> RepoOwner -> [T.Text] -> T.Text -> IO [(Either Error RepoWebhook)]
createWebhooks auth repoOwner' repos publicUri = do
  reposToCreate <- (filterM (\repo -> not . snd <$> webhookExists auth repoOwner' repo publicUri) repos)
  mapM (\repo -> createRepoWebhook' auth repoOwner' (T.unpack repo) brandNewRepoHook) reposToCreate
    where
      brandNewRepoHook = NewRepoWebhook {
        newRepoWebhookName = "web"
       ,newRepoWebhookConfig = M.fromList [("url", T.unpack publicUri), ("content_type", "json") ]
       ,newRepoWebhookEvents = Just [WebhookPullRequestEvent]
       ,newRepoWebhookActive = Just True
     }

-- | Check is a webhook with a given URI exists on a repo
webhookExists :: GithubAuth -> RepoOwner -> T.Text -> T.Text -> IO (T.Text, Bool)
webhookExists auth repoOwner' repoName' publicUri = do
  possibleWebhooks <- webhooksFor' auth repoOwner' (T.unpack repoName')
  case possibleWebhooks of
    (Left _) -> return (repoName', False)
    (Right webhooks) -> return $ (repoName', (any (\wh -> publicUri == T.pack (repoWebhookUrl wh)) webhooks))

-- | Close a pull request
closePullRequest :: GithubAuth -> DetailedPullRequest -> IO (Either Error DetailedPullRequest)
closePullRequest auth dpr =
  updatePullRequest auth repoOwner' repoName' prNumber EditPullRequest { editPullRequestTitle = Nothing, editPullRequestBody = Nothing, editPullRequestState = Just EditPullRequestStateClosed }
  where repoOwner' = getOwner dpr
        repoName' = getRepoName dpr
        prNumber = detailedPullRequestNumber dpr

-- | Leave a comment on a pull request
commentOnPullRequest :: GithubAuth -> DetailedPullRequest -> T.Text -> IO (Either Error Comment)
commentOnPullRequest auth dpr commentBody' = createComment auth user' repo' issue' (T.unpack commentBody')
  where user' = getOwner dpr
        repo' = getRepoName dpr
        issue' = getIssueNumber dpr

-- | Given the details of a pull request extract its repository owner
getOwner :: DetailedPullRequest -> String
getOwner dpr = case (repoOwner . pullRequestCommitRepo . detailedPullRequestHead) dpr of
                    GithubUser _ userOwnerLoging _ _ _ -> userOwnerLoging
                    GithubOrganization _ orgOwnerLoging _ _ -> orgOwnerLoging

-- | Given the details of a pull request extract ist repository name
getRepoName :: DetailedPullRequest -> String
getRepoName = repoName . pullRequestCommitRepo . detailedPullRequestHead

-- | Given a pull request extract its repository name
getPullRequestRepoName :: PullRequest -> String
getPullRequestRepoName = githubOwnerLogin . pullRequestUser

-- | Given the details of a pull request extract its issue number
getIssueNumber :: DetailedPullRequest -> Int
getIssueNumber dpr = (read . T.unpack $ parts L.!! (L.length parts - 1) :: Int)
  where parts = T.splitOn (T.pack "/") (T.pack $ detailedPullRequestIssueUrl dpr)

-- | Fetches the open pull requests from a repo and perform the
-- adecuate action on it
processRepos :: GithubAuth -> RepoOwner -> [T.Text] ->  IO [Either Error DetailedPullRequest]
processRepos auth repoOwner' repos' = do
  pullRequests <- sequence $ map (\repo -> pullRequestsFor' (Just auth) repoOwner' (T.unpack repo)) repos'
  let openPullRequests = filter (\pr -> pullRequestState pr == "open") (concat $ rights pullRequests)
  openPullRequestsDetails <- sequence $ map (\pr -> pullRequest' (Just auth) repoOwner' (getPullRequestRepoName pr) (pullRequestId pr)) openPullRequests
  let orderedOpenPullRequestsDetails = L.sortBy (compare `F.on` (\pr -> detailedPullRequestCreatedAt pr)) (rights openPullRequestsDetails)
  typedPullRequests <- sequence $  map (\pr -> getPullRequestType pr) orderedOpenPullRequestsDetails
  results <- sequence $ map (\pr -> processPullRequestType auth pr) typedPullRequests
  return results
