module Postmon 
    ( fetchPosts
    ) where

import Network.HTTP.Client

gateway = "http://api.postmon.com.br/v1/rastreio/ect/"

