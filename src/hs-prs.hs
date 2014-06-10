{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import qualified Data.Text as T
import qualified Data.Aeson.Types as Json

main :: IO ()
main = scotty 3000 $ do
  get (literal "/status") $ do
    (json . Json.String . T.pack) "Working fine ..."
