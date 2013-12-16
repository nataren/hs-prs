{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty
import Data.Aeson ((.:), (.:?), decode, FromJSON(..), Value(..))
import Control.Applicative ((<$>), (<*>))
import qualified Data.ByteString.Lazy.Char8 as BS
import Network.HTTP.Types
import Network.Wai.Middleware.RequestLogger

main = scotty 3000 $ do
  middleware logStdoutDev
  
  get "/status" $ do
    json [(0::Int) .. 10]

  post "/notify" $ do
    json ("notified" :: String)
  
  notFound $ do 
    json ("there is no such route" :: String)
  
  