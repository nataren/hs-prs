{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import qualified Data.Text as T
import qualified Network.HTTP.Types as H


main :: IO ()
main = scotty 3000 $ do
  get "/status" $ do
    (json . T.pack) "Working fine ..."

  post "/notify" $ do
    status H.notImplemented501
