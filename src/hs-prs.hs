{-# LANGUAGE OverloadedStrings #-}

import qualified Web.Scotty as S
import qualified Data.Text as T
import Data.Aeson
import Github.Data
import PullRequest.Utils

main :: IO ()
main = S.scotty 3000 $ do
  S.get "/status" $ do
    S.json . T.pack $ "Working fine ..."

  S.post "/pr/notify" $ do
    b <- S.body
    let event = decode b :: Maybe PullRequestEvent
    case event of
      Just ev -> S.json $ show . getPullRequestTypeFromEvent $ ev
      Nothing -> S.json . T.pack $ ""
