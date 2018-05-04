{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Network.Bot
    ( run
    ) where

import Network.Postmon
import Data.Text
import Data.Text.Manipulate as L
import Pipes
import Data.Monoid
import Network.Discord

reply :: Message -> Text -> Effect DiscordM ()
reply Message{messageChannel=chan} cont = fetch' $ CreateMessage chan cont Nothing

replyGreet :: User -> Text
replyGreet User{userName=author} = ":heartpulse: Guten Morgen, " <>  pack author <> "! :heartpulse:"

posts :: Text -> IO Text
posts code = do 
  content <- fetchPosts $ unpack code
  case content of
    Just a -> return . pack $ show a
    Nothing -> return "Nothing found actually"

replyPost :: Text -> IO Text
replyPost msg = posts (L.dropWord msg)  
  
run :: IO ()
run = runBot (Bot "TOKEN") $ do
  with ReadyEvent $ \(Init v u _ _ _) ->
    liftIO . putStrLn $ "Connected to gateway v" ++ show v ++ " as user " ++ show u

  with MessageCreateEvent $ \msg@Message{..} -> do
    when ("baka!" `isPrefixOf` messageContent && (not . userIsBot $ messageAuthor)) $ do
      liftIO (replyPost messageContent) >>= reply msg

    when ("greet!" `isPrefixOf` messageContent && (not . userIsBot $ messageAuthor)) $ do
      reply msg $ replyGreet messageAuthor
      