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

data Posts =
  Posts { codigo  :: !Text
         , servico :: !Text
           } deriving (Show, Generic)

instance FromJSON Posts
instance ToJSON Posts           

gateway :: String -> String
gateway = (++) "http://api.postmon.com.br/v1/rastreio/ect/"

fetchPosts :: String -> IO String
fetchPosts code = do
  response <- simpleHttp $ gateway code
  return $ L8.unpack response
  