{-# LANGUAGE OverloadedStrings #-}

module Network.Postmon 
    ( fetchPosts
    ) where

import Control.Exception (try)
import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Simple

gateway :: String -> String
gateway = (++) "http://api.postmon.com.br/v1/rastreio/ect/"

fetchPosts :: String -> IO String
fetchPosts code = do
  let url = gateway code
  initReq <- parseRequest url
  response <- httpLbs initReq
  return $ L8.unpack . getResponseBody $ response
 
