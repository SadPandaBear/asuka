{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module DiscordWSS
    ( runExample
    ) where

import Data.Text
import Pipes

import Network.Discord

reply :: Message -> Text -> Effect DiscordM ()
reply Message{messageChannel=chan} cont = fetch' $ CreateMessage chan cont Nothing

runExample :: IO ()
runExample = runBot (Bot "NDQxMDI3NTcyNDk1Njc5NTA5.DcqgpA.kQ3BpczNN5UxGeny-Ph340ztnHo") $ do
  with ReadyEvent $ \(Init v u _ _ _) ->
    liftIO . putStrLn $ "Connected to gateway v" ++ show v ++ " as user " ++ show u

  with MessageCreateEvent $ \msg@Message{..} -> do
    when ("Ping" `isPrefixOf` messageContent && (not . userIsBot $ messageAuthor)) $
      reply msg "Pong!"