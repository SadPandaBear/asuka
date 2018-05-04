{-# LANGUAGE OverloadedStrings, DeriveGeneric, InstanceSigs #-}

module Network.Postmon 
    ( fetchPosts
    ) where

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
  toJSON :: History -> Value
  toJSON history = object 
    [ "data" .= toJSON (date history)
    , "local" .= toJSON (local history)
    , "situacao" .= toJSON (situacao history)
    ]

data Posts = Posts { 
    historico :: History -- TODO: Fix this shit
  , codigo :: String
  , servico :: String
  } deriving (Show, Generic)

instance FromJSON Posts
instance ToJSON Posts where
  toJSON posts = object
    [ "codigo" .= toJSON (codigo posts)
    , "servico" .= toJSON (servico posts)
    , "historico" .= toJSON (historico posts)
    ]       

gateway :: String -> String
gateway = (++) "http://api.postmon.com.br/v1/rastreio/ect/"

fetchPosts :: String -> IO (Maybe Posts)
fetchPosts code = do
  response <- simpleHttp $ gateway code
  let req = decode response :: Maybe Posts
  return req
  