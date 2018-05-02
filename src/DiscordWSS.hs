{-# LANGUAGE OverloadedStrings #-}

-- this is a simple example don't take it seriously

module DiscordWSS 
    ( runExample
    ) where

import Control.Monad.State
import Data.Maybe

import Network.URL
import Pipes

import Network.Discord.Types
import Network.Discord.Gateway

data PutStrClient = PsClient
instance Client PutStrClient where
  getAuth _ = Bot "NDQxMDI3NTcyNDk1Njc5NTA5.DcqV2Q.xt8xVWnLJ7TOXJimUMzaXwE-5KM"

runExample :: IO ()
runExample = runWebsocket (fromJust $ importURL "wss://gateway.discord.gg") PsClient $ do
  st <- get
  for (eventCore (getWebSocket st))
    (\pl -> lift . liftIO $ print (pl:: Event))