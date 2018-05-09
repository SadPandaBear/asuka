{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Bot
    ( run
    ) where

import Network.Postmon
import Data.Text
import Data.Text.Manipulate as L
import Pipes
import Data.Monoid
import Network.Discord
import System.Environment

reply :: Message -> Text -> Effect DiscordM ()
reply Message{messageChannel=chan} cont = fetch' $ CreateMessage chan cont Nothing

replyGreet :: User -> Text
replyGreet User{userName=author} = "Guten Morgen, " <> pack author <> "! :heart:"

replyPost :: (Text -> IO Text)
replyPost = fetchPosts . L.dropWord 
  
run :: IO ()
run = do
  botToken <- lookupEnv "BOT_TOKEN"
  case botToken of 
    Just t -> runBot (Bot t) $ do
      with ReadyEvent $ \(Init v u _ _ _) ->
        liftIO . putStrLn $ "Connected to gateway v" ++ show v ++ " as user " ++ show u

      with MessageCreateEvent $ \msg@Message{..} -> do
        when ("baka!" `isPrefixOf` messageContent && (not . userIsBot $ messageAuthor)) $ do
          liftIO (replyPost messageContent) >>= reply msg

        when ("greet!" `isPrefixOf` messageContent && (not . userIsBot $ messageAuthor)) $ do
          reply msg $ replyGreet messageAuthor
    Nothing -> liftIO . putStrLn $ "BOT_TOKEN couldn't be retrieved. Check your environment variables."