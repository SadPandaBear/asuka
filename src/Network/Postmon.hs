{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Network.Postmon 
    ( fetchPosts
    ) where

import Control.Exception (try)
import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Conduit
import Data.Aeson
import Data.Text
import GHC.Generics

data History = History { 
    date :: String
  , local :: String
  , situacao :: String
  } deriving (Show, Generic)

instance FromJSON History
instance ToJSON History where
  toJSON dt = object 
    [ "data" .= date dt
    , "local" .= local dt
    , "situacao" .= situacao dt
    ]

data Posts = Posts { 
    historico :: !Array -- TODO: Fix this shit
  , codigo  :: String
  , servico :: String
  } deriving (Show, Generic)

instance FromJSON Posts
instance ToJSON Posts         

gateway :: String -> String
gateway = (++) "http://api.postmon.com.br/v1/rastreio/ect/"

fetchPosts :: String -> IO (Maybe Posts)
fetchPosts code = do
  response <- simpleHttp $ gateway code
  let req = decode response :: Maybe Posts
  return req
  